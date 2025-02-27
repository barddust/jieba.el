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

(defun jieba--json-read-string (s)
  (if (fboundp 'json-parse-string)
      (json-parse-string s
                         :object-type 'plist
                         :false-object :json-false
                         :null-object nil)
    (let ((json-object-type 'plist)
          (json-false :json-false)
          (json-null nil))
      (json-read-from-string s))))

;;; JSONRPC setup

(defvar jieba--current-python-conn nil)

(defclass jieba-python-connection (jsonrpc-process-connection) ()
  "A connection based on stdio to contact with jieba server.")

(cl-defmethod jsonrpc-connection-send ((conn jieba-python-connection)
                                       &rest args
                                       &key method &allow-other-keys)
  "Override send method, because we just send JSON without HTTP headers."
  (when method
    (plist-put args :method
               (cond ((keywordp method) (substring (symbol-name method) 1))
                     ((and method (symbolp method)) (symbol-name method)))))
  (let* ((message `(:jsonrpc "2.0" ,@args))
         (json (jsonrpc--json-encode message)))
    (process-send-string
     (jsonrpc--process conn)
     json)))

(cl-defmethod initialize-instance ((conn jieba-python-connection) _slots)
  (cl-call-next-method)
  ;; Set a new process filter for `jieba-python-connection'.
  ;; Because our messages don't contain HTTP headers.
  (let ((proc (jsonrpc--process conn)))
    (when proc
      (set-process-filter proc #'jieba--process-filter))))

(defun jieba--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point))
        (let ((json-message (condition-case-unless-debug oops
                                (jieba--json-read-string string)
                              (error
                               (jsonrpc--warn "Invalid JSON: %s %s"
                                              (cdr oops) (buffer-string))
                               nil)))
              (conn (process-get proc 'jsonrpc-connection)))
          (when json-message
            (with-temp-buffer
              (jsonrpc-connection-receive conn
                                          json-message))))))))

(defun jieba--python-connect ()
  "Connect to our python jieba server."
  (let* ((name "JIEBA-SERVER")
         (default-directory (jieba--current-dir))
         (service (alist-get 'python jieba-server-alist))
         (conn (jieba-python-connection
                :process (lambda ()
                           (open-network-stream
                            name
                            name
                            (car service)
                            (cdr service)
                            :noquery t
                            :coding 'utf-8-emacs-unix)))))
    ;; Ask server to load default dict.
    (message "aaa")
    (jsonrpc-notify conn :hello nil)
    (setq jieba--current-python-conn conn)))

;;; Backend implementation

(cl-defmethod jieba--initialize-backend ((_backend (eql python)))
  (jieba--python-connect))

(cl-defmethod jieba--shutdown-backend ((_backend (eql python)))
  (jsonrpc-shutdown jieba--current-python-conn))

(cl-defmethod jieba--backend-available? ((_backend (eql python)))
  (and (cl-typep jieba--current-python-conn 'jieba-python-connection)
       (jsonrpc-running-p jieba--current-python-conn)))

(cl-defmethod jieba-load-dict ((_backend (eql python)) dicts)
  (jsonrpc-async-request jieba--current-python-conn
                         :loadDict (vconcat dicts)))

(cl-defmethod jieba-do-split ((_backend (eql python)) str)
  (jsonrpc-request jieba--current-python-conn :split str))

(provide 'jieba-python)

;;; jieba-python.el ends here
