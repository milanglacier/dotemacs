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
(defun my/mu4e-open-link-via-eww (msg &optional arg)
    "If point is on a link, open this link via `eww'. Otherwise open
this email via `eww'"
    (if-let ((link-at-point (get-text-property (point) 'shr-url)))

            ;; if point is on a link, open this link via eww
            (eww link-at-point arg)

        ;; else open the email via eww
        (let ((browse-url-browser-function
               (lambda (url &optional _rest)
                   (eww url))))
            (mu4e-action-view-in-browser msg))))

(provide 'my-email-autoloads)
;;; my-email-autoloads.el ends here
