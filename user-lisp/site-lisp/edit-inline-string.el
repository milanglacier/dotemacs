;;; edit-inline-string.el -*- lexical-binding: t; -*-

;; Author: Milan Glacier <dev@milanglacier.com>
;; Maintainer: Milan Glacier <dev@milanglacier.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29") (edit-indirect "0.1.13"))

;;; Commentary:

;; This package provides a command to edit the contents of a string
;; literal in a separate, temporary buffer.

;;; Code:

(defun edit-inline-string-treesit-get-string-range ()
    (let ((node (treesit-node-at (point)))
          (query "((string_content) @str)"))
        (treesit-query-range node query)))

(defvar edit-inline-string-hook-guess-mode-functions '(edit-inline-string-detect-sql)
    "Hooks to detect the mode of the temporary buffer for
edit-inline-string. These functions take one argument which is the content
of the temp buffer.")

(defun edit-inline-string-detect-sql (content)
    (cond
     ((string-match "\\`[ \t\n]*--[ \t]*[sS][qQ][lL]" content) 'sql-mode)
     ((string-match "\\`[ \t\n]*/\\*.*[sS][qQ][lL].*\\*/" content) 'sql-mode)
     (t nil)))

;;;###autoload
(defun edit-inline-string ()
    "Edit the embedded code within a separate buffer."
    (interactive)
    (require 'edit-indirect)
    (when-let* ((range (edit-inline-string-treesit-get-string-range))
                (beg (caar range))
                (end (cdar range))
                (content (buffer-substring-no-properties beg end))
                (edit-indirect-guess-mode-function
                 (lambda (&rest _)
                     (funcall
                      (or (run-hook-with-args-until-success 'edit-inline-string-hook-guess-mode-functions content)
                          'normal-mode)))))
        (edit-indirect-region beg end t)))

(provide 'edit-inline-string)
