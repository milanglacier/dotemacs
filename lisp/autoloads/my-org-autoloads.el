;;; my-org-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/load-org-extensions-idly ()
    "Some important variables from other org extensions are not autoloaded.
You may feel annoying if you want to use them but find a void variable.
(e.g. you want to call `org-open-at-point' on a timestamp)"
    (let ((org-packages '(ob org-capture org-agenda)))
        (dolist (pkg org-packages)
            (require pkg))))

;;;###autoload
(defun my/org-capture-bubble-tea-template (letter desc headings template &rest properties)
    `(,letter ,desc table-line
              (file+olp ,my/org-capture-bubble-tea-live-file
                        ,@headings ,(format-time-string "%Y") ,(format-time-string "%B"))
              ,template ,@properties :unnarrowed t))
;; HACK: here without `:unnarrowed t', the org-capture will
;; automatically insert a new line after the table. That is, if you
;; use the org-capture to create `table-line' entries a lot, your file
;; will get more and more blank lines, which makes your file
;; unreadable.  Plus, if you have your table with `#+TBLFM', then this
;; new line inserted by org capture breaks your table structure such
;; that all of your fields that are calculated by the formula becomes
;; unreachable. Because there should be new line before a `#+TBLFM'
;; and the table itself.

;; TODO: I don't find a way to disable this behavior yet. Ask for the
;; mailing-list.

;;;###autoload
(defun my/org-agenda-visited-all-directories ()
    "Org agenda need to visted all files listed in `org-agenda-files'
to create the view, which is expensive. By default I will only list a
small portion of files to be searched.  This function searches all the
files in the org-directory to create the org-agenda view"
    (interactive)
    (let* ((default-directory org-directory)
           (directories-string (shell-command-to-string "find * -type d -print0"))
           ;; the literal null character will cause git to wrongly
           ;; consider this file as a binary file. Use the `kbd' to
           ;; get the internal representation of the null character
           (directories (split-string directories-string (kbd "^@") nil))
           (org-agenda-files (mapcar (lambda (x)
                                         (file-name-concat org-directory x))
                                     directories)))
        (call-interactively #'org-agenda)))

;;;###autoload
(defun my/org-bubble-tea-get-end-of-play-time (start)
    "After clocking in to record the start time of playing with bubble tea,
when clocking out, use this function to automatically update the table."
    (save-excursion
        (goto-char (org-find-olp
                    `(,(buffer-name) "play" ,(format-time-string "%Y") ,(format-time-string "%B"))))
        (re-search-forward (replace-regexp-in-string "[][]" "" start)
                           ;; [ and ] are regex reserved identifers,
                           ;; need to escape them.
                           nil
                           t)
        (let ((start-of-end-time (1- (re-search-forward "\\[" nil t)))
              ;; when use `buffer-substring-no-properties', the point
              ;; is left exclusive but right inclusive, so need to subtract
              ;; the point of the start by 1.
              (end-of-end-time (re-search-forward "\\]" nil t)))
            (buffer-substring-no-properties start-of-end-time end-of-end-time))))

(provide 'my-org-autoloads)
;;; my-org-autoloads.el ends here
