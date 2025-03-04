;;; jieba.el  --- Use nodejieba chinese segmentation in Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: bardust0@outlook.com
;; URL: https://github.com/barddust/jieba.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.2") (jsonrpc "1.0.7") (websocket "1.15"))
;; Keywords: chinese

;; This file is NOT a part of GNU Emacs.

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

;; This package use JSONRPC protocol to contact with a simple wrapper
;; of nodejieba, A chinese word segmentation tool.

;;; Code:

(require 'eieio)

(eval-when-compile
  (require 'cl-lib))

;;; Customize

(defgroup jieba ()
  ""
  :group 'chinese
  :prefix "jieba-")

(defcustom jieba-server-alist
  '((python . ("127.0.0.1" . 58291))
    (node . 2932))
  ""
  :type 'alist
  :group 'jieba)

(defcustom jieba-current-backend 'python
  "The Jieba backend in using."
  :type 'symbol
  :group 'jieba)

(defcustom jieba-data-dir (expand-file-name "jieba" user-emacs-directory)
  "Directory to save jieba cache and custom build scripts."
  :type 'string
  :group 'jieba)

(defcustom jieba-build-server-automatically nil
  "Try to run build server script after enabling jieba-mode."
  :type 'boolean
  :group 'jieba)

;;; Utils

(defconst jieba--current-dir
  (file-name-directory
   (cond
    (load-in-progress load-file-name)
    ((and (boundp 'byte-compile-current-file)
          byte-compile-current-file)
     byte-compile-current-file)
    (t (buffer-file-name))))
  "Directory of jieba.")

;;; Backend Access API

(cl-defgeneric jieba-do-split (backend str))

(cl-defgeneric jieba-load-dict (backend dicts))

(cl-defgeneric jieba--initialize-backend (_backend)
  nil)

(cl-defgeneric jieba--shutdown-backend (_backend)
  nil)

(cl-defgeneric jieba--backend-available? (backend))

(defun jieba-ensure (&optional interactive-restart?)
  (interactive "P")
  (if (not (jieba--backend-available? jieba-current-backend))
      (progn
        (jieba--initialize-backend jieba-current-backend))
    (when (and
           interactive-restart?
           (y-or-n-p
            "Jieba backend is running now, do you want to restart it?"))
      (jieba--shutdown-backend jieba-current-backend)
      (jieba--initialize-backend jieba-current-backend))))

(defun jieba--assert-server ()
  "Assert the server is running, throw an error when assertion failed."
  (or (jieba--backend-available? jieba-current-backend)
      (error "[JIEBA] Current backend: %s is not available!"
             jieba-current-backend)))

(defun jieba--server-built-p (backend)
  "Check if Server has been built for BACKEND."
  (let ((flag (expand-file-name
               (format ".build.%s" backend)
               jieba-data-dir)))
    (file-exists-p flag)))

(defun jieba-build-server (&optional all-p)
  (interactive "P")
  (let* ((servers (map-keys jieba-server-alist))
         (backends
          (if all-p servers
            (cl-remove-duplicates
             (completing-read-multiple
                       "Backend: "
                       servers
                       nil t)))))
    (dolist (backend backends)
      (unless (jieba--server-built-p backend)
        (let* ((script-name (format "%s.build.sh" backend))
               (sys-script (expand-file-name script-name
                                             jieba--current-dir))
               (user-script (expand-file-name script-name jieba-data-dir)))

          (cl-loop for script in (list (expand-file-name script-name jieba--current-dir)
                                       (expand-file-name script-name jieba-data-dir))
                   when (file-exists-p script) do
                   (progn (async-shell-command (format "sh %s" script))
                          (with-temp-file (expand-file-name (format ".build.%s" backend) jieba-data-dir))
                          (cl-return))))))))

;;; Export function

(defvar jieba--single-chinese-char-re "\\cC")

(defun jieba-split-chinese-word (str)
  (jieba-do-split jieba-current-backend str))

(defsubst jieba-chinese-word? (s)
  "Return t when S is a real chinese word (All its chars are chinese char.)"
  (and (string-match-p (format "%s\\{%d\\}"
                               jieba--single-chinese-char-re
                               (length s)) s)
       t))

(defalias 'jieba-chinese-word-p 'jieba-chinese-word?)

(defun jieba--segment-atpt-bounds (backward?)
  (let ((pnt (point)) (beg (point)) (end (point)))
    (save-excursion
      (if backward?
          (progn (backward-word)
                 (setq beg (point))
                 (when (looking-at jieba--single-chinese-char-re)
                   (forward-word)
                   (setq end (if (< pnt (point)) pnt (point)))))
        (forward-word)
        (setq end (point))
        (when (looking-back jieba--single-chinese-char-re pnt)
          (backward-word)
          (setq beg (if (> pnt (point)) pnt (point))))))
    (cons beg end)))

(defun jieba--chinese-word-atpt-bounds (beg end)
  (jieba--assert-server)
  (let ((word (buffer-substring-no-properties beg end)))
    (when (jieba-chinese-word? word)
      (let ((cur (point))
            (index beg)
            (old-index beg))
        (cl-block retval
          (mapc (lambda (x)
                  (cl-incf index (length x))
                  (cond
                   ((or (< cur index) (= index end))
                    (cl-return-from retval (cons old-index index)))
                   (t
                    (setq old-index index))))
                (jieba-split-chinese-word word)))))))


(defun jieba--move-chinese-word (backward?)
  (pcase (jieba--segment-atpt-bounds backward?)
    (`(,beg . ,end)
     (pcase (jieba--chinese-word-atpt-bounds beg end)
       (`(,start . ,finish)
        (setq beg start)
        (setq end finish)))
     (goto-char (if backward? beg end)))))


;;;###autoload
(defun jieba-forward-word (&optional arg)
  (interactive "p")
  (setq arg (or arg 1))
  (let ((backward? (< arg 0)))
    (dotimes (_ (abs arg))
      (jieba--move-chinese-word backward?))))

;;;###autoload
(defun jieba-backward-word (&optional arg)
  (interactive "p")
  (setq arg (or arg 1))
  (jieba-forward-word (- arg)))

;;;###autoload
(defun jieba-kill-word (arg)
  (interactive "p")
  (kill-region (point) (progn (jieba-forward-word arg) (point))))

;;;###autoload
(defun jieba-backward-kill-word (arg)
  (interactive "p")
  (jieba-kill-word (- arg)))

;;;###autoload
(defun jieba-mark-word (&optional arg)
  (interactive "p")
  (set-mark (point))
  (jieba-forward-word arg))

;;; Minor mode

;;;###autoload
(defvar jieba-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap forward-word] #'jieba-forward-word)
    (define-key map [remap backward-word] #'jieba-backward-word)
    (define-key map [remap kill-word] #'jieba-kill-word)
    (define-key map [remap backward-kill-word] #'jieba-backward-kill-word)
    map))

;;;###autoload
(define-minor-mode jieba-mode
  ""
  :global t
  :keymap jieba-mode-map
  :lighter " Jieba"
  (when jieba-mode
    (when jieba-build-server-automatically
      (jieba-build-server t))
    (jieba-ensure t)))

(provide 'jieba)

(cl-eval-when (load eval)
  (require 'jieba-python))

;;; jieba.el ends here
