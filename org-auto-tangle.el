;;; org-auto-tangle.el --- Automatically and Asynchronously tangles org files on save -*- lexical-binding: t; -*-

;; Author: Yilkal Argaw
;; URL: https://github.com/yilkalargaw/auto-tangle
;; Version: 0.0.1
;; Keywords: outlines
;; Package-Requires: ((emacs "24.1") (async "1.9.3"))

;; This file is not part of GNU Emacs

;; Copyright <2021> <Yilkal Argaw>
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;
;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Commentary:

;; It is common to want to tangle org files everytime you save your changes,
;; especially for common tangled init files. So this program allows you to
;; do so using #+auto_tangle option in an org file. It also does so asynchronously
;; so it does let you Emacs session hang

;;; Usage:

;; - Add #+auto_tangle:t to your tangled org file
;; - Make changes to the emacs file and save your changes


;;; Code:

(require 'async)


(defun org-auto-tangle-find-value (buffer)
  "Search the `auto_tangle' property in BUFFER and extracts it when found."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "^#\\+auto_tangle: \\(.*\\)" nil :noerror)
	  (match-string 1))))))

(defun org-auto-tangle-async (file)
  "Invoke `org-babel-tangle-file' asynchronously on FILE."
  (message "Tangling %s..." (buffer-file-name))
  (async-start
   (let ((args (list file)))
     `(lambda ()
	(require 'org)
	(let ((start-time (current-time)))
	  (apply #'org-babel-tangle-file ',args)
	  (format "%.2f" (float-time (time-since start-time))))))
   (let ((message-string (format "Tangling %S completed after " file)))
     `(lambda (tangle-time)
	(message (concat ,message-string
			 (format "%s seconds" tangle-time)))))))

(defun org-auto-tangle-tangle-if-tag-exists ()
  "Check if the #+auto_tangle option exists and call org-auto-tangle-async if it exists."
  (when (and (string= (buffer-local-value 'major-mode (current-buffer)) "org-mode")
	     (org-auto-tangle-find-value (current-buffer))
	     (not (string= (org-auto-tangle-find-value(current-buffer)) "nil")))
    (org-auto-tangle-async (buffer-file-name))))

(define-minor-mode org-auto-tangle-mode
  "Automatically tangle org-mode files with the option #+auto_tangle: t."
  :lighter " org-a-t"

  (if org-auto-tangle-mode
	      (add-hook 'after-save-hook 'org-auto-tangle-tangle-if-tag-exists
			nil 'local)
    (remove-hook 'after-save-hook 'org-auto-tangle-tangle-if-tag-exists)))

(provide 'org-auto-tangle)

;;; org-auto-tangle.el ends here
