;;; jieba-python.el  --- python jieba backend  -*- lexical-binding: t -*-

;; Copyright (C) 2024 FingerKnight

;; Author: FingerKnight <mrdust1880@outlook.com>
;; Keywords: chinese

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'jieba)
(require 'jsonrpc)
(require 'websocket)

(defcustom jieba-python-cache-file (expand-file-name "jieba.cache" user-emacs-directory)
  "Cache file to use for Python Jieba")

;;; JSONRPC setup
(defvar jieba--current-python-conn nil)

(defclass jieba-python-connection (jsonrpc-connection)
  ((ws :accessor jsonrpc-python-ws))
  "A connection based on stdio to contact with jieba server.")

(cl-defmethod initialize-instance ((conn jieba-python-connection) slots)
  (cl-call-next-method)
  )

(cl-defmethod jsonrpc-connection-send ((conn jieba-python-connection)
                                       &rest args
                                       &key _id method _params _result _error _partial)
  "copy from jsonrpc-process-connection's implementation"
  (when method
    (plist-put args :method
               (cond ((keywordp method) (substring (symbol-name method) 1))
                     ((and method (symbolp method)) (symbol-name method)))))
  ;; Adapt to python jsonrpc server
  (if (plist-get args :params)
      (plist-put args :params `[,(plist-get args :params)])
    (plist-put args :params []))
  (let* ((message `(:jsonrpc "2.0" ,@args))
         (json (json-serialize message)))
    (websocket-send-text (jsonrpc-python-ws conn) json)))

(defun jieba--python-connect ()
  "Connect to python jieba server."
  (setq jieba--current-python-conn (jieba-python-connection))
  (let* ((name "JIEBA-SERVER")
         (service (alist-get 'python jieba-server-alist))
         (uri (format "ws://%s:%d" (car service) (cdr service))))
    (setf (jsonrpc-python-ws jieba--current-python-conn)
        (websocket-open uri :on-message
                                (lambda (_ws frame)
                                  (let* ((json (with-temp-buffer
                                                 (insert (websocket-frame-payload frame))
                                                 (goto-char (point-min))
                                                 (jsonrpc--json-read))))
                                    (jsonrpc-connection-receive jieba--current-python-conn json))))))
  (jsonrpc-request jieba--current-python-conn :hello jieba-python-cache-file))

;;; Backend implementation
(cl-defmethod jieba--backend-available? ((_backend (eql python)))
  (and jieba--current-python-conn
       (websocket-openp (jsonrpc-python-ws jieba--current-python-conn))))

(cl-defmethod jieba--initialize-backend ((_backend (eql python)))
  (jieba--python-connect))

(cl-defmethod jieba--shutdown-backend ((_backend (eql python)))
  (websocket-close (jsonrpc-python-ws jieba--current-python-conn)))

(cl-defmethod jieba-load-dict ((_backend (eql python)) dicts)
  (jsonrpc-async-request jieba--current-python-conn :loadDict (vconcat dicts)))

(cl-defmethod jieba-do-split ((_backend (eql python)) str)
  (jsonrpc-request jieba--current-python-conn :split str))


(provide 'jieba-python)

;;; jieba-python.el ends here
