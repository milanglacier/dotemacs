;;; my-email-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/mu4e-enter-func (use-org-msg-mode &optional additional-func)
    (lambda ()
        (org-msg-mode use-org-msg-mode)
        (when additional-func
            (funcall additional-func))))

;;;###autoload
(defun my/mu4e-leave-func (&optional additional-func)
    (lambda ()
        (mu4e-clear-caches)
        (when additional-func
            (funcall additional-func))))

;;;###autoload
(defun my/mu4e-match-func (prefix)
    (lambda (msg)
        (when msg
            (string-prefix-p prefix (mu4e-message-field msg :maildir) t))))

;;;###autoload
(defun my/mu4e-set-mail-line-wrap ()
              (setq-local fill-column 72
                          fill-flowed-encode-column 72))

(provide 'my-email-autoloads)
;;; my-email-autoloads.el ends here
