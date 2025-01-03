;;; lib-minibuffer.el -*- lexical-binding: t; -*-

;;;###autoload
(defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))

;;;###autoload
(defun mg-completion-in-region (&rest args)
    (apply (if vertico-mode
                   #'consult-completion-in-region
               #'completion--in-region)
           args))

;; copied from doomemacs
;;;###autoload (autoload #'mg-evil-delete-in-wgrep "lib-minibuffer" nil t)
(evil-define-operator mg-evil-delete-in-wgrep (beg end type register yank-handler)
    "A wrapper around `evil-delete' for `wgrep' buffers that will invoke
`wgrep-mark-deletion' on lines you try to delete."
    (interactive "<R><x><y>")
    (condition-case _ex
            (evil-delete beg end type register yank-handler)
        ('text-read-only
         (evil-apply-on-block
          (lambda (beg _)
              (goto-char beg)
              (call-interactively #'wgrep-mark-deletion))
          beg (1- end) nil))))

(provide 'lib-minibuffer)
;;; lib-minibuffer ends here
