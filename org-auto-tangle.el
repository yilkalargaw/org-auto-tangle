;;; org-auto-tangle.el --- Automatically and Asynchronously tangles org files on save -*- lexical-binding: t; -*-

;; Author: Yilkal Argaw <yilkalargawworkneh@gmail.com>
;; URL: https://github.com/yilkalargaw/org-auto-tangle
;; Version: 0.6.0
;; Keywords: outlines
;; Package-Requires: ((emacs "24.1") (async "1.9.3"))

;; This file is not part of GNU Emacs

;; Copyright (c) 2021, Yilkal Argaw
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; It is common to want to tangle org files everytime you save your changes,
;; especially for tangled init files. So this program allows you to
;; do so using #+auto_tangle option in an org file. It also accomplishes 
;; this feat asynchronously so it does not let you Emacs session hang.

;;; Usage:

;; - Add #+auto_tangle: t to your tangled org file
;; - Make changes to the emacs file and save your changes


;;; Code:

(require 'async)
(require 'cl-lib)
(require 'org)
(require 'ox)               ; org-export--parse-option-keyword

(defcustom org-auto-tangle-default nil
  "Default behavior of org-auto-tangle.

If nil (default), auto-tangle will only happen on buffers with
the `#+auto_tangle: t' keyword. If t, auto-tangle will happen on
all Org buffers unless `#+auto_tangle: nil' is set."
  :group 'org-auto-tangle
  :type 'boolean)

(defcustom org-auto-tangle-babel-safelist '()
  "List of full path of files for which code blocks need to be evaluated.

By default, code blocks are not evaluated during the auto-tangle to avoid
possible code execution from unstrusted source. To enable code blocks evaluation
for a specific file, add its full path to this list."
  :group 'org-auto-tangle
  :type '(repeat (file :tag "Full file path")))

(defun org-auto-tangle-find-value (buffer)
  "Return the value of the `auto_tangle' keyword in BUFFER."
  (with-current-buffer buffer
    (cdr (assoc "AUTO_TANGLE" (org-collect-keywords '("AUTO_TANGLE"))))))

;; This is modeled after `org-export-filters-alist', since it is
;; passed to `org-export--parse-option-keyword'.
(defconst org-auto-tangle-options-alist
  '((:with-vars nil "vars" org-auto-tangle-with-vars))
  "Alist between auto-tangle properties and ways to set them.

The key of the alist is the property name, and the value is a list
like (KEYWORD OPTION DEFAULT BEHAVIOR) where:

KEYWORD is a string representing a buffer keyword, or nil.  Each
  property defined this way can also be set, during subtree
  export, through a headline property named after the keyword
  with the \"EXPORT_\" prefix (i.e. DATE keyword and EXPORT_DATE
  property).
OPTION is a string that could be found in an #+OPTIONS: line.
DEFAULT is the default value for the property.
BEHAVIOR determines how Org should handle multiple keywords for
  the same property.  It is a symbol among:
  nil       Keep old value and discard the new one.
  t         Replace old value with the new one.
  `space'   Concatenate the values, separating them with a space.
  `newline' Concatenate the values, separating them with
            a newline.
  `split'   Split values at white spaces, and cons them to the
            previous list.
  `parse'   Parse value as a list of strings and Org objects,
            which can then be transcoded with, e.g.,
            `org-export-data'.  It implies `space' behavior.

Values set through KEYWORD and OPTION have precedence over
DEFAULT.")

(defgroup org-auto-tangle nil
  "Automatic tangling of `org-mode' documents."
  :tag "Org Auto Tangle"
  :group 'org-babel)

(defcustom org-auto-tangle-with-vars nil
  "Non-nil means pass VARS variables to the async tangling process.

This option can also be set with the AUTO_TANGLE keyword,
e.g. \"vars:calendar-latitude\".

The `org-src-preserve-indentation', `org-babel-pre-tangle-hook',
and `org-babel-post-tangle-hook' variables are automatically
preserved and do not need to be listed here."
  :group 'org-auto-tangle
  :type '(repeat (symbol :tag "Variable name")))

(defun org-auto-tangle--get-inbuffer-options ()
  "Return current buffer auto-tangle options, as a plist.

Assume buffer is in Org mode.  Narrowing, if any, is ignored."
  (let (plist)
    ;; Read options in the current buffer and return value.
    (dolist (entry (org-collect-keywords '("AUTO_TANGLE")) plist)
      (pcase entry
        (`("AUTO_TANGLE" . ,values)
         (setq plist
               (apply #'org-combine-plists
                      plist
                      (mapcar (lambda (v)
                                (let ((org-export-options-alist)))
                                (org-auto-tangle--parse-auto-tangle-keyword v))
                              values))))))))

(defun org-auto-tangle--parse-auto-tangle-keyword (auto-tangle)
  "Parse an AUTO-TANGLE line and return values as a plist."
  (let ((org-export-options-alist org-auto-tangle-options-alist))
    (org-export--parse-option-keyword auto-tangle)))

(defun org-auto-tangle-async (file)
  "Invoke `org-babel-tangle-file' asynchronously on FILE."
  (message "Tangling %s..." (buffer-file-name))
  (async-start
   (let* ((buf-vars (plist-get (org-auto-tangle--get-inbuffer-options)
                               :with-vars))
          (with-vars (if buf-vars
                         (mapcar #'intern
                                 (org-uniquify (org-split-string
                                                (symbol-name buf-vars) ":")))
                       org-auto-tangle-with-vars))
          (preserved (mapcar (lambda (v)
                               (cons v (symbol-value v)))
                             (append '(org-src-preserve-indentation
                                       org-babel-pre-tangle-hook
                                       org-babel-post-tangle-hook)
                                     with-vars)))
          (evaluate (not (member file org-auto-tangle-babel-safelist))))
     (lambda ()
       (require 'org)
       (let ((start-time (current-time))
             (non-essential t)
             (org-confirm-babel-evaluate evaluate))
         (cl-progv (mapcar #'car preserved) (mapcar #'cdr preserved)
           (org-babel-tangle-file file))
         (format "%.2f" (float-time (time-since start-time))))))
   (let ((message-string (format "Tangling %S completed after" file)))
     (lambda (tangle-time)
       (message "%s %s seconds" message-string tangle-time)))))

(defun org-auto-tangle-tangle-if-needed ()
  "Call org-auto-tangle-async if needed.

Tangle will happen depending on the value of
`org-auto-tangle-default' and on the presence and value of the
`#+auto_tangle' keyword in the current buffer. If present,
`#+auto_tangle' always overrides `org-auto-tangle-default'."
  (let ((auto-tangle-kw (org-auto-tangle-find-value (current-buffer))))
    (when (and (derived-mode-p 'org-mode)
               (if auto-tangle-kw
                   (not (member "nil" auto-tangle-kw))
                 org-auto-tangle-default))
      (org-auto-tangle-async (buffer-file-name)))))

;;;###autoload
(define-minor-mode org-auto-tangle-mode
  "Automatically tangle org-mode files with the option #+auto_tangle: t."
  :lighter " org-a-t"

  (if org-auto-tangle-mode
      (add-hook 'after-save-hook #'org-auto-tangle-tangle-if-needed
                nil 'local)
    (remove-hook 'after-save-hook #'org-auto-tangle-tangle-if-needed 'local)))

(provide 'org-auto-tangle)

;;; org-auto-tangle.el ends here
