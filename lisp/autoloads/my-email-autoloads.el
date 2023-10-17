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

;; WIP: there are too many corner cases unresolved
;; TODO: very beginning stage

(defvar-local my$mu4e-thread-lines-alist ()
    "An alist constitutes of `(thread_id line-num-1 line-num-2 line-num-3 ...)',
each of which is the `line-number-at-pos' of a message belonging to
that thread.")

(defvar-local my$mu4e-thread-unread-msg-alist ()
    "An alist constitutes of `(thread_id . unread-msg-num)'")

(defvar-local my$mu4e-thread-overlays-alist ()
    "An alist constitutes of `(thread_id . overlay)'")

(defvar-local my$mu4e-global-fold-state nil
    "Global state of whether threads should be all folded or not")

(defvar-local my$mu4e-folded-thread-override ()
    "A list of threads that should be restored to folded status after refresh.")

(defvar-local my$mu4e-unfolded-thread-override ()
    "A list of threads that should be restored to unfolded status after refresh.")

(defface my&mu4e-unread-thread
    '((t :inherit (mu4e-highlight-face)))
    "face for folded thread containing unread messages.")

(defface my&mu4e-read-thread
    '((t :inherit (mu4e-highlight-face)))
    "face for folded thread containing no unread messages.")

(defun my:mu4e-thread-set-lines ()
    (my~mu4e-unfold-all-threads)
    (setq-local my$mu4e-thread-lines-alist nil)
    (setq-local my$mu4e-thread-overlays-alist nil)
    (setq-local my$mu4e-thread-unread-msg-alist nil)

    (mu4e-headers-for-each
     (lambda (msg)
         (let* ((cur-thread-id (mu4e~headers-get-thread-info msg 'thread-id))
                (cur-thread-lines (alist-get cur-thread-id my$mu4e-thread-lines-alist nil nil #'equal))
                (unread-p (memq 'unread (mu4e-message-field msg :flags))))
             (if cur-thread-lines
                     (progn
                         (push (line-number-at-pos)
                               (alist-get cur-thread-id my$mu4e-thread-lines-alist nil nil #'equal))
                         (when unread-p
                             (setf (alist-get cur-thread-id my$mu4e-thread-unread-msg-alist nil nil #'equal)
                                   (1+ (alist-get cur-thread-id my$mu4e-thread-unread-msg-alist nil nil #'equal)))))
                 (progn
                     (push `(,cur-thread-id ,(line-number-at-pos))
                           my$mu4e-thread-lines-alist)
                     (push `(,cur-thread-id . ,(if unread-p 1 0))
                           my$mu4e-thread-unread-msg-alist)))))))

(defun my:mu4e-get-prepended-whitespaces-for-folded-text ()
    ;; the last fields of `mu4e-headers-fields' is usually `subject'
    ;; which should not be limited by length, thus its field value is
    ;; nil. We remove the last nil element and get the summation of
    ;; the length of other fields.
    (let ((white-spaces (+ 2
                           ;; the beginning 2 spaces are reserved for mu4e for special purposes
                           (length (butlast mu4e-headers-fields))
                           (apply
                            #'+
                            (butlast (mapcar #'cdr mu4e-headers-fields))))))
        (cl-loop for i from 1 to white-spaces concat " ")))

(defun my~mu4e-fold-thread-at-point (&optional no-record-fold-status)
    "Fold the thread to which the message at point belongs. If
NO-RECORD-FOLD-STATUS is t, the fold status for current thread will
not be recorded."
    (interactive)
    (let* ((msg (mu4e-message-at-point))
           (cur-thread-id (mu4e~headers-get-thread-info msg 'thread-id))
           (over-lay))
        (when-let* ((is-not-folded (not (alist-get cur-thread-id my$mu4e-thread-overlays-alist nil nil #'equal)))
                    (thread-lines (alist-get cur-thread-id my$mu4e-thread-lines-alist nil nil #'equal))
                    (num-of-messages (length thread-lines))
                    (unread-messages (alist-get cur-thread-id my$mu4e-thread-unread-msg-alist nil nil #'equal))
                    (more-than-1-messages (> num-of-messages 1))
                    (folded-text (concat
                                  (my:mu4e-get-prepended-whitespaces-for-folded-text)
                                  (if (> unread-messages 0)
                                          (propertize
                                           (format
                                            "--- thread with %s messages, %d unread ---"
                                            num-of-messages
                                            unread-messages)
                                           'face
                                           'my&mu4e-unread-thread)
                                      (propertize
                                       (format
                                        "--- thread with %s messages ---"
                                        num-of-messages)
                                       'face
                                       'my&mu4e-read-thread)))))

            (save-excursion
                (setq over-lay (make-overlay
                                (1+ (seq-min (mapcar
                                              (lambda (x) (goto-line x) (line-end-position))
                                              thread-lines)))
                                (seq-max (mapcar
                                          (lambda (x) (goto-line x) (line-end-position))
                                          thread-lines)))))

            (unless no-record-fold-status
                (push cur-thread-id my$mu4e-folded-thread-override)
                (setq-local my$mu4e-unfolded-thread-override (delete cur-thread-id my$mu4e-unfolded-thread-override)))

            (setf (alist-get cur-thread-id my$mu4e-thread-overlays-alist nil nil #'equal) over-lay)
            (overlay-put over-lay 'display folded-text))))

(defun my~mu4e-unfold-thread-at-point ()
    "Unfold the thread to which the message at point belongs"
    (interactive)
    (let* ((msg (mu4e-message-at-point))
           (cur-thread-id (mu4e~headers-get-thread-info msg 'thread-id))
           (over-lay))
        (when-let* ((over-lay (alist-get cur-thread-id my$mu4e-thread-overlays-alist nil nil #'equal)))
            (if (overlayp over-lay)
                    (progn
                        (beginning-of-line)
                        (delete-overlay over-lay)
                        (setf (alist-get cur-thread-id my$mu4e-thread-overlays-alist nil nil #'equal) nil))
                (setf (alist-get cur-thread-id my$mu4e-thread-overlays-alist nil nil #'equal) nil))
            (push cur-thread-id my$mu4e-unfolded-thread-override)
            (setq-local my$mu4e-folded-thread-override (delete cur-thread-id my$mu4e-folded-thread-override))
            t
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
    (setq-local my$mu4e-thread-overlays-alist nil)
    (setq-local my$mu4e-global-fold-state 'unfolded)
    (setq-local my$mu4e-folded-thread-override nil)
    (setq-local my$mu4e-unfolded-thread-override nil))

(defun my:mu4e-override-folded-thread (override-threads)
    "Override the global fold state for threads that should be folded"
    (save-excursion
        (dolist (thread-id override-threads)
            (when-let* ((thread-lines (alist-get thread-id my$mu4e-thread-lines-alist nil nil #'equal))
                        (tail-of-thread (car thread-lines)))
                (goto-line tail-of-thread)
                (my~mu4e-fold-thread-at-point)))))

(defun my:mu4e-override-unfolded-thread (override-threads)
    "Override the global fold state for threads that should be unfolded"
    (save-excursion
        (dolist (thread-id override-threads)
            (when-let* ((thread-lines (alist-get thread-id my$mu4e-thread-lines-alist nil nil #'equal))
                        (tail-of-thread (car thread-lines)))
                (goto-line tail-of-thread)
                (my~mu4e-unfold-thread-at-point)))))


(defun my~mu4e-fold-all-threads ()
    "fold all threads in current buffer."
    (interactive)
    (mu4e-headers-for-each
     (lambda (msg)
         (my~mu4e-fold-thread-at-point t)))
    (setq-local my$mu4e-global-fold-state 'folded)
    (setq-local my$mu4e-folded-thread-override nil)
    (setq-local my$mu4e-unfolded-thread-override nil))

(defun my:mu4e-thread-folding-mode-setup ()
    (my:mu4e-thread-set-lines))

(defun my:mu4e-thread-folding-mode-unsetup ()
    (my~mu4e-unfold-all-threads)
    (setq-local my$mu4e-thread-overlays-alist nil)
    (setq-local my$mu4e-thread-lines-alist nil)
    (setq-local my$mu4e-thread-unread-msg-alist nil)
    (setq-local my$mu4e-global-fold-state nil)
    (setq-local my$mu4e-folded-thread-override nil)
    (setq-local my$mu4e-unfolded-thread-override nil))

(defun my:mu4e-unfold-thread-before-opening-message (old-fun &rest args)
    ;; HACK: It seems that the overlay will be displayed incorrectly
    ;; after you openning a message. I have no idea why this happens.
    ;; So I have to unfold current thread before opening a message.
    ;; Besides, I think it makes sense to unfold a thread since you
    ;; are reading the message within the thread.
    (my~mu4e-unfold-thread-at-point)
    (apply old-fun args))

(defun my:mu4e-refresh-fold-after-executing-mark (old-fun &rest args)
    (let ((fold-status my$mu4e-global-fold-state)
          (thread-folded-override my$mu4e-folded-thread-override)
          (thread-unfolded-override my$mu4e-unfolded-thread-override)
          (marks-num (mu4e-mark-marks-num)))
        (apply old-fun args)
        (my~mu4e-unfold-all-threads)
        ;; HACK: The mail update for mu4e occurs asynchronously,
        ;; therefore, an immediate update of the fold status following
        ;; command execution is impossible. However, I don't know how
        ;; to determine when the update concludes. Therefore, I've
        ;; resorted to using a timer to approximate this timeline.
        (run-with-idle-timer
         (cond
          ((< marks-num 10) 0.3)
          ((< marks-num 50) 0.5)
          ((< marks-num 100) 1)
          (t 2))
         nil
         (lambda ()
             (my:mu4e-thread-set-lines)
             (pcase fold-status
                 ('folded (my~mu4e-fold-all-threads)
                          (my:mu4e-override-unfolded-thread thread-unfolded-override))
                 ('unfolded (my~mu4e-unfold-all-threads)
                            (my:mu4e-override-folded-thread thread-folded-override)))))))

;;;###autoload
(define-minor-mode my~mu4e-thread-folding-mode
    "Enable thread folding for mu4e."
    :global t
    :init-value nil
    (if my~mu4e-thread-folding-mode
            (progn
                (add-hook 'mu4e-headers-found-hook #'my:mu4e-thread-folding-mode-setup)
                (advice-add 'mu4e-headers-view-message :around #'my:mu4e-unfold-thread-before-opening-message)
                (advice-add 'mu4e-mark-execute-all :around #'my:mu4e-refresh-fold-after-executing-mark)
                )
        (progn
            (when (get-buffer "*mu4e-headers*")
                (with-current-buffer "*mu4e-headers*"
                    (my:mu4e-thread-folding-mode-unsetup)))
            (remove-hook 'mu4e-headers-found-hook #'my:mu4e-thread-folding-mode-setup)
            (advice-remove 'mu4e-headers-view-message #'my:mu4e-unfold-thread-before-opening-message)
            (advice-remove 'mu4e-mark-execute-all #'my:mu4e-refresh-fold-after-executing-mark)
            )))

(provide 'my-email-autoloads)
;;; my-email-autoloads.el ends here
