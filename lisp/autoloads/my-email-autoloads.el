;;; my-email-autoloads.el -*- lexical-binding: t; -*-

(defvar my$notmuch-deleted-tags "action-delete")

;;;###autoload
(defun my:notmuch-setup ()
    "Modified from `evil-collection-notmuch', but remove those keybindings I do not need. Especially for archiving."
    (evil-collection-require 'notmuch)

    (evil-collection-inhibit-insert-state 'notmuch-show-mode-map)
    (evil-collection-inhibit-insert-state 'notmuch-search-mode-map)
    (evil-collection-inhibit-insert-state 'notmuch-tree-mode-map)

    (evil-set-initial-state 'notmuch-show-mode 'normal)
    (evil-set-initial-state 'notmuch-search-mode 'normal)
    (evil-set-initial-state 'notmuch-hello-mode 'normal)
    (evil-set-initial-state 'notmuch-tree-mode 'normal)


    (general-define-key
     :states 'normal
     :keymaps 'notmuch-common-keymap
     "g?" #'notmuch-help
     "q" #'notmuch-bury-or-kill-this-buffer
     "s" #'notmuch-search
     "S" #'notmuch-tree
     "C" #'notmuch-mua-new-mail           ; like mu4e
     "cc" #'notmuch-mua-new-mail          ; like mu4e
     "gr" #'notmuch-refresh-this-buffer
     "gA" #'notmuch-refresh-all-buffers
     "gR" #'notmuch-poll-and-refresh-this-buffer
     "J" #'notmuch-jump-search)

    (general-define-key
     :states 'normal
     :keymaps 'notmuch-hello-mode-map
     "TAB" 'widget-forward
     "S-TAB" 'widget-backward
     "RET" #'my~notmuch-hello-ret)

    ;; I usually don't archive email, so I delete all the keymaps related to archive
    (general-define-key
     :states 'normal
     :keymaps 'notmuch-show-mode-map
     "gd" #'goto-address-at-point
     "p"  #'notmuch-show-save-attachments ; like mu4e
     "S"  #'notmuch-show-filter-thread
     "K"  #'notmuch-tag-jump
     "C"  #'notmuch-mua-new-mail ; like mu4e
     "cc" #'notmuch-mua-new-mail ; like mu4e
     "cr" #'notmuch-show-reply-sender ; like mu4e
     "cR" #'notmuch-show-reply
     "cf" #'notmuch-show-forward-message ; forward only current message
     ;; forward the entire thread if on conversation mode, otherwise forward current message
     "cF" #'notmuch-show-forward-open-messages
     "zv" #'notmuch-tree-from-show-current-query ; like mu4e-conversation
     "<" #'notmuch-show-toggle-thread-indentation
     "D" #'my~notmuch-search-toggle-deleted
     "=" #'evil-collection-notmuch-show-toggle-flagged
     "H" #'notmuch-show-toggle-visibility-headers
     "gj" #'notmuch-show-next-open-message
     "gk" #'notmuch-show-previous-open-message
     "]]" #'notmuch-show-next-message
     "[[" #'notmuch-show-previous-message
     "][" #'notmuch-show-next-thread-show
     "[]" #'notmuch-show-previous-thread-show
     "|" #'notmuch-show-pipe-message
     "*" #'notmuch-show-tag-all
     "-" #'notmuch-show-remove-tag
     "+" #'notmuch-show-add-tag
     "TAB" #'notmuch-show-next-button
     "S-TAB" #'notmuch-show-previous-button
     "RET" #'notmuch-show-toggle-message
     "SPC SPC" 'notmuch-show-part-map
     "gs" 'notmuch-show-stash-map
     )

    (add-to-list 'evil-fold-list
                 '((notmuch-show-mode)
                   :open-all my~notmuch-show-open-entire-thread
                   :close-all my~notmuch-show-close-entire-thred
                   :toggle notmuch-show-toggle-message))

    ;; I usually don't archive email, so I delete all the keymaps related to archive
    (general-define-key
     :states 'normal
     :keymaps 'notmuch-search-mode-map
     "J" #'notmuch-jump-search
     "S" #'notmuch-search-filter
     "K" #'notmuch-tag-jump ; apply +tags with popup menu to select to selected messages, <C-u> to apply -tags.
     "O" #'notmuch-search-toggle-order
     "zv" #'notmuch-tree-from-search-current-query
     "*" #'notmuch-search-tag-all
     "D" #'my~notmuch-search-toggle-deleted
     "!" #'evil-collection-notmuch-search-toggle-unread
     "F" #'evil-collection-notmuch-search-toggle-flagged
     "q" #'notmuch-bury-or-kill-this-buffer
     "cc" #'notmuch-mua-new-mail ; like mu4e
     "cr" #'notmuch-search-reply-to-thread-sender
     "cR" #'notmuch-search-reply-to-thread
     "t" #'notmuch-search-filter-by-tag
     "-" #'notmuch-search-remove-tag
     "+" #'notmuch-search-add-tag
     "RET" #'notmuch-search-show-thread
     "gs" 'notmuch-search-stash-map
     )

    ;; I usually don't archive email, so I delete all the keymaps related to archive
    (general-define-key
     :states 'visual
     :keymaps 'notmuch-search-mode-map
     "-" #'notmuch-search-remove-tag
     "+" #'notmuch-search-add-tag
     "K" #'notmuch-tag-jump ; apply +tags with popup menu to select to selected messages, <C-u> to apply -tags.
     )

    (general-define-key
     :states 'normal
     :keymaps 'notmuch-tree-mode-map
     [mouse-1] nil
     "g?" #'notmuch-help
     "q" #'notmuch-tree-quit
     "S" #'notmuch-tree-to-search
     "C" #'notmuch-mua-new-mail ; like mu4e
     "cc" #'notmuch-mua-new-mail ; like mu4e
     "J" #'notmuch-jump-search
     "zv" #'notmuch-search-from-tree-current-query ; like mu4e-conversation
     "cr" #'notmuch-show-reply-sender ; like mu4e
     "cR" #'notmuch-show-reply
     "cf" #'notmuch-tree-forward-message
     "!" #'evil-collection-notmuch-tree-toggle-unread
     "=" #'evil-collection-notmuch-tree-toggle-flagged
     "K" #'notmuch-tag-jump
     "RET" #'notmuch-tree-show-message
     "s" #'notmuch-tree-to-tree
     "gj" #'notmuch-tree-next-matching-message
     "gk" #'notmuch-tree-prev-matching-message
     "]]" #'notmuch-tree-next-message
     "[[" #'notmuch-tree-prev-message
     "[]" #'notmuch-tree-prev-thread
     "][" #'notmuch-tree-next-thread
     "|" #'notmuch-show-pipe-message
     "-" #'notmuch-tree-remove-tag
     "+" #'notmuch-tree-add-tag
     "*" #'notmuch-tree-tag-thread
     "e" #'notmuch-tree-resume-message
     )
    )

(defun my~notmuch-hello-ret ()
    (interactive)
    (if (widget-field-at (point))
            (call-interactively #'widget-field-activate)
        (call-interactively #'widget-button-press)))

(defun my~notmuch-search-toggle-deleted ()
    "Toggle deleted tag for message."
    (interactive)
    (evil-collection-notmuch-toggle-tag my$notmuch-deleted-tags "search" 'notmuch-search-next-thread))

;; copied from doomemacs
(defun my:notmuch-search-show-thread (fn &rest args)
    "Give email buffers a sane name so they can be targeted via
`display-buffer-alist'"
    (cl-letf* ((orig-notmuch-show
                (symbol-function #'notmuch-show))
               ((symbol-function #'notmuch-show)
                (lambda (thread-id &optional elide-toggle parent-buffer query-context buffer-name)
                    (funcall orig-notmuch-show
                             thread-id elide-toggle parent-buffer query-context
                             (format "*nm-msg:%s*" (substring buffer-name 1 -1))))))
        (apply fn args)))

(defun my:notmuch-ensure-field-truncation (fn &rest args)
    "Ensure the field get truncated appropriately"
    (require 's)
    (cl-letf* ((orig-format
                (symbol-function #'format))
               ((symbol-function #'format)
                (lambda (string &rest objects)
                    (when-let* ((truncation-width
                                 (s-match (rx "%-" (group (+ num)) "s")
                                          string))
                                (truncation-width (nth 1 truncation-width))
                                (truncation-width (string-to-number truncation-width)))
                        (setq objects
                              (mapcar
                               (lambda (x)
                                   (if (and (stringp x)
                                            (> (string-width x) truncation-width))
                                           (truncate-string-to-width x truncation-width)
                                       x))
                               objects))
                        )
                    (apply orig-format string objects))))
        (apply fn args)))

(defun my~notmuch-show-open-entire-thread ()
    "Show all messages in current thread."
    (interactive)
    (let ((current-prefix-arg nil))
        (notmuch-show-open-or-close-all)))

(defun my~notmuch-show-close-entire-thred ()
    "Hide all messages in current thread."
    (interactive)
    (let ((current-prefix-arg t))
        (notmuch-show-open-or-close-all)))

(provide 'my-email-autoloads)
;;; my-email-autoloads.el ends here
