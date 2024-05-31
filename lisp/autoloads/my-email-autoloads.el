;;; my-email-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my:mu4e-enter-func (use-org-msg-mode &optional additional-func)
    (lambda ()
        (org-msg-mode use-org-msg-mode)
        (when additional-func
            (funcall additional-func))))

;;;###autoload
(defun my:mu4e-leave-func (&optional additional-func)
    (lambda ()
        (when additional-func
            (funcall additional-func))))

;;;###autoload
(defun my:mu4e-match-func (prefix)
    (lambda (msg)
        (when msg
            (string-prefix-p prefix (mu4e-message-field msg :maildir) t))))

;;;###autoload
(defun my:mu4e-open-link-via-eww (msg)
    "If point is on a link, open this link via `eww'. Otherwise open
this email via `eww'"
    (if-let ((link-at-point (or (get-text-property (point) 'shr-url)
                                (get-text-property (point) 'mu4e-url))))

            ;; if point is on a link, open this link via eww
            (eww link-at-point)

        ;; else open the email via eww
        (let ((browse-url-browser-function
               (lambda (url &rest _rest)
                   (eww url))))
            (mu4e-action-view-in-browser msg))))

(defun my:mu4e-action-view-in-xwidget (msg)
    "If point is on a link, open this link via `xwidget'. Otherwise open
this email via `xwidget'"
    (unless (fboundp 'xwidget-webkit-browse-url)
        (mu4e-error "No xwidget support available"))
    (if-let ((link-at-point (or (get-text-property (point) 'shr-url)
                                (get-text-property (point) 'mu4e-url))))

            ;; if point is on a link, open this link via eww
            (xwidget-webkit-browse-url link-at-point)
        (let ((browse-url-handlers nil)
              (browse-url-browser-function
               (lambda (url &rest _rest)
                   (xwidget-webkit-browse-url url))))
            (mu4e-action-view-in-browser msg))))


(defun my:mu4e-head-of-thread-p ()
    "Non-nil means current message is the first message of a thread."
    (save-excursion
        (let ((current-msg (mu4e-message-at-point))
              (prev-msg (progn
                            (mu4e-headers-prev)
                            (mu4e-message-at-point))))
            (or (bobp)
                (not (equal (mu4e~headers-get-thread-info current-msg 'thread-id)
                            (mu4e~headers-get-thread-info prev-msg 'thread-id)))))))

(defun my:mu4e-tail-of-thread-p ()
    "Non-nil means current message is the last message of a thread."
    (save-excursion
        (if-let ((current-msg (mu4e-message-at-point))
                 (next-msg (progn
                               (mu4e-headers-next)
                               (and (progn (goto-char (line-end-position))
                                           (not (eobp)))
                                    (mu4e-message-at-point)))))
                (not (equal (mu4e~headers-get-thread-info current-msg 'thread-id)
                            (mu4e~headers-get-thread-info next-msg 'thread-id)))
            ;; if next-msg is nil, this means we have reach the last
            ;; line of mu4e-headers.
            t)))

(cl-defun my:mu4e-goto-next-thread-start ()
    (let* ((msg (mu4e-message-at-point))
           (thread-id (mu4e~headers-get-thread-info msg 'thread-id)))
        (mu4e-headers-next)
        (when (save-excursion
                  (goto-char (line-end-position))
                  (eobp))
            (mu4e-headers-prev)
            (cl-return-from my:mu4e-goto-next-thread-start))
        (when (equal thread-id
                     (mu4e~headers-get-thread-info
                      (mu4e-message-at-point)
                      'thread-id))
            (my:mu4e-goto-next-thread-start))))

(defun my:mu4e-goto-prev-thread-end ()
    (let* ((msg (mu4e-message-at-point))
           (thread-id (mu4e~headers-get-thread-info msg 'thread-id))
           (cur-thread-id thread-id))
        (while (and (not (bobp))
                    (equal thread-id cur-thread-id))
            (mu4e-headers-prev)
            (setq cur-thread-id (mu4e~headers-get-thread-info
                                 (mu4e-message-at-point)
                                 'thread-id)))))

(defun my:mu4e-goto-current-thread-start ()
    (my:mu4e-goto-prev-thread-end)
    (unless (bobp)
        (mu4e-headers-next)))

(defun my:mu4e-goto-current-thread-end ()
    (my:mu4e-goto-next-thread-start)
    (unless (save-excursion
                (mu4e-headers-next)
                (goto-char (line-end-position))
                (eobp))
        (mu4e-headers-prev)))

(defun my~mu4e-thread-forward-start ()
    "Go to the start of next thread. Analagous to `]]' in vim."
    (interactive)
    (my:mu4e-goto-next-thread-start))

(defun my~mu4e-thread-forward-end ()
    "Go to the tail of current thread or tail of next thread if point
is on the end of current thread. Analagous to `][' in vim."
    (interactive)
    (if (my:mu4e-tail-of-thread-p)
            (progn
                (my:mu4e-goto-next-thread-start)
                (my:mu4e-goto-current-thread-end))
        (my:mu4e-goto-current-thread-end)))

(defun my~mu4e-thread-backward-start ()
    "Go to the head of current thread or head of previous thread if point
is on the start of current thread. Analagous to `[[' in vim."
    (interactive)
    (if (my:mu4e-head-of-thread-p)
            (progn (my:mu4e-goto-prev-thread-end)
                   (my:mu4e-goto-current-thread-start))
        (my:mu4e-goto-current-thread-start)))

(defun my~mu4e-thread-backward-end ()
    "Go to the tail of previous thread. Analagous to `[]' in vim."
    (interactive)
    (my:mu4e-goto-prev-thread-end))

(defun my~mu4e-view-thread-forward ()
    "Go to prev thread or start of current thread"
    (interactive)
    (if (version< mu4e-mu-version "1.10")
            (mu4e~view-quit-buffer)
        (mu4e-view-quit)
        (switch-to-buffer "*mu4e-headers*"))
    ;; FIXME: in mu4e 1.10+: upon calling `mu4e-view-quit', the value
    ;; of `current-buffer' temporarily changes to "*mu4e-main*"
    ;; instead of "*mu4e-headers*". Although the interactive behavior
    ;; remains the same (as if we return to the "*mu4e-headers*"
    ;; buffer immediately), `my~mu4e-thread-forward-start' function
    ;; requires the buffer to be "*mu4e-headers*". This could be a
    ;; side effect of using `kill-buffer-and-window'. To ensure that
    ;; we switch back to the "*mu4e-headers*" buffer, we need to
    ;; enforce it explicitly.
    (my~mu4e-thread-forward-start)
    (mu4e-headers-view-message))

(defun my~mu4e-view-thread-backward ()
    "Go to prev thread or start of current thread"
    (interactive)
    (if (version< mu4e-mu-version "1.10")
            (mu4e~view-quit-buffer)
        (mu4e-view-quit)
        (switch-to-buffer "*mu4e-headers*"))
    (my~mu4e-thread-backward-start)
    (mu4e-headers-view-message))

(provide 'my-email-autoloads)
;;; my-email-autoloads.el ends here
