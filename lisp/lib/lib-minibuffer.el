;;; lib-minibuffer.el -*- lexical-binding: t; -*-

;;;###autoload
(defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (string-replace "[ \t]*" "" crm-separator)
                  (car args))
          (cdr args)))

;;;###autoload
(defun mg-disable-gc ()
    (setq gc-cons-threshold most-positive-fixnum))

(defun mg-restore-gc ()
    ;; set gc-cons-threshold back to 64M
    (setq gc-cons-threshold 67108864))

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
