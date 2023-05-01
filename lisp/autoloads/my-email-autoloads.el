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
        (mu4e-clear-caches)
        (when additional-func
            (funcall additional-func))))

;;;###autoload
(defun my:mu4e-match-func (prefix)
    (lambda (msg)
        (when msg
            (string-prefix-p prefix (mu4e-message-field msg :maildir) t))))

;;;###autoload
(defun my:mu4e-open-link-via-eww (msg &optional arg)
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

(setq my$mu4e-enable-thread-folding nil)

;; WIP: there are too many corner cases unresolved
;; TODO: very beginning stage
(when my$mu4e-enable-thread-folding

    (defvar my$mu4e-thread-points-alist ()
        "An alist constitutes of `(thread_id point1 point2 point3 ...)',
where each point is the `line-end-position' of a message belonging to
that thread.")

    (defvar my$mu4e-thread-overlays-alist ()
        "An alist constitutes of `(thread_id . overlay)'")

    (defface my&mu4e-unread-thread '((t :inherit (mu4e-unread-face hl-line))) "")

    (defface my&mu4e-read-thread '((t :inherit (mu4e-header-face hl-line))) "")

    (defmacro my%mu4e-thread-alist-get (key alist &optional remove default)
        "Similar to `alist-get' except that using `equal' as the comparison method."
        `(alist-get ,key ,alist ,default ,remove #'equal))

    (defun my:mu4e-thread-set-points ()
        (my~mu4e-unfold-all-threads)
        (setq my$mu4e-thread-points-alist nil)
        (setq my$mu4e-thread-overlays-alist nil)

        (mu4e-headers-for-each
         (lambda (msg)
             (let ((cur-thread-id (mu4e~headers-get-thread-info msg 'thread-id)))
                 (if (my%mu4e-thread-alist-get cur-thread-id my$mu4e-thread-points-alist)
                         (push (line-end-position)
                               (my%mu4e-thread-alist-get cur-thread-id my$mu4e-thread-points-alist))
                     (push `(,cur-thread-id ,(line-end-position))
                           my$mu4e-thread-points-alist))
                 ))))

    (defun my:mu4e-get-prepended-whitespaces-for-folded-text ()
        ;; the last fields of `mu4e-headers-fields' is usually `subject'
        ;; which should not be limited by length, thus its field value is
        ;; nil. We remove the last nil element and get the summation of
        ;; the length of other fields.
        (let ((white-spaces (apply
                             #'+
                             (butlast (mapcar #'cdr mu4e-headers-fields)))))
            (cl-loop for i from 1 to white-spaces concat " ")))

    (defun my~mu4e-fold-thread-at-point ()
        "Fold the thread to which the message at point belongs"
        (interactive)
        (let* ((msg (mu4e-message-at-point))
               (cur-thread-id (mu4e~headers-get-thread-info msg 'thread-id))
               (over-lay))
            (when-let* ((is-not-folded (not (my%mu4e-thread-alist-get cur-thread-id my$mu4e-thread-overlays-alist)))
                        (thread-points (my%mu4e-thread-alist-get cur-thread-id my$mu4e-thread-points-alist))
                        (num-of-messages (length thread-points))
                        (more-than-1-messages (> num-of-messages 1))
                        (folded-text (concat "\n"
                                             (my:mu4e-get-prepended-whitespaces-for-folded-text)
                                             (format "--- %s messages folded ---" num-of-messages))))
                ;; overlay is left and right inclusive thus the first
                ;; message of thread should be intact, and the last
                ;; message of thread should be completely folded
                (setq over-lay (make-overlay
                                (seq-min thread-points)
                                (seq-max thread-points)))
                (if (my%mu4e-thread-alist-get cur-thread-id my$mu4e-thread-overlays-alist)
                        (setf (my%mu4e-thread-alist-get cur-thread-id my$mu4e-thread-overlays-alist)
                              over-lay)
                    (push `(,cur-thread-id . ,over-lay) my$mu4e-thread-overlays-alist))
                (overlay-put over-lay 'display (propertize folded-text 'face 'my&mu4e-unread-thread))
                (overlay-put over-lay 'overlay over-lay))))

    (defun my~mu4e-unfold-thread-at-point ()
        "Unfold the thread to which the message at point belongs"
        (interactive)
        (let* ((msg (mu4e-message-at-point))
               (cur-thread-id (mu4e~headers-get-thread-info msg 'thread-id))
               (over-lay))
            (when-let* ((over-lay (my%mu4e-thread-alist-get cur-thread-id my$mu4e-thread-overlays-alist)))
                (if (overlayp over-lay)
                        (progn
                            (delete-overlay over-lay)
                            (setf (my%mu4e-thread-alist-get cur-thread-id my$mu4e-thread-overlays-alist) nil))
                    (setf (my%mu4e-thread-alist-get cur-thread-id my$mu4e-thread-overlays-alist) nil))
                )))

    (defun my~mu4e-toggle-thread-folding-at-point ()
        "Toggle the folding state of the thread to which message at point belongs"
        (interactive)
        (unless (my~mu4e-unfold-thread-at-point)
            (my~mu4e-fold-thread-at-point)))

    (defun my~mu4e-unfold-all-threads ()
        "Unfold all threads in current buffer."
        (interactive)
        (dolist (thread-over-lay my$mu4e-thread-overlays-alist)
            (when (overlayp (cdr thread-over-lay))
                (delete-overlay (cdr thread-over-lay))))
        (setq my$mu4e-thread-overlays-alist nil))

    (define-minor-mode my~mu4e-thread-folding-mode
        "Enable thread folding for mu4e."
        :global nil
        :init-value nil

        (if my~mu4e-thread-folding-mode
                (progn
                    (make-variable-buffer-local 'my$mu4e-thread-overlays-alist)
                    (make-variable-buffer-local 'my$mu4e-thread-points-alist)
                    (my:mu4e-thread-set-points))
            (progn
                (my~mu4e-unfold-all-threads)
                (setq-local my$mu4e-thread-overlays-alist nil)
                (setq-local my$mu4e-thread-points-alist nil))
            )
        )

    )

(provide 'my-email-autoloads)
;;; my-email-autoloads.el ends here
