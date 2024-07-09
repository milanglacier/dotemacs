;;; my-email-autoloads.el -*- lexical-binding: t; -*-

(defvar my$notmuch-deleted-tags "action-delete")

;;;###autoload
(defun my:notmuch-setup ()
    "Modified from `evil-collection-notmuch', but remove those keybindings I do not need. Especially for archiving."
    (require 'evil-collection-notmuch
             (expand-file-name "modes/notmuch/evil-collection-notmuch" evil-collection-base-dir))

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
     "cf" #'notmuch-show-forward-message
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


    ;; I usually don't archive email, so I delete all the keymaps related to archive
    (general-define-key
     :states 'normal
     :keymaps 'notmuch-search-mode-map
     "cC" #'compose-mail-other-frame
     "J" #'notmuch-jump-search
     "S" #'notmuch-search-filter
     "K" #'notmuch-tag-jump ; apply +tags with popup menu to select to selected messages, <C-u> to apply -tags.
     "O" #'notmuch-search-toggle-order
     "zv" #'notmuch-tree-from-search-current-query
     "*" #'notmuch-search-tag-all
     "cc" #'compose-mail                ; like mu4e
     "D" #'my~notmuch-search-toggle-deleted
     "!" #'evil-collection-notmuch-search-toggle-unread
     "F" #'evil-collection-notmuch-search-toggle-flagged
     "q" #'notmuch-bury-or-kill-this-buffer
     "cr" #'notmuch-search-reply-to-thread-sender
     "cR" #'notmuch-search-reply-to-thread
     "t" #'notmuch-search-filter-by-tag
     [mouse-1] #'notmuch-search-show-thread
     "-" #'notmuch-search-remove-tag
     "+" #'notmuch-search-add-tag
     "RET" #'notmuch-search-show-thread
     "gs" 'notmuch-search-stash-map
     )


    ;; I usually don't archive email, so I delete all the keymaps related to archive
    (general-define-key
     :states 'visual
     "-" #'notmuch-search-remove-tag
     "+" #'notmuch-search-add-tag
     "K" #'notmuch-tag-jump ; apply +tags with popup menu to select to selected messages, <C-u> to apply -tags.
     )

    (general-define-key
     :states 'normal
     :keymaps 'notmuch-tree-mode-map
     "g?" #'notmuch-help
     "q" #'notmuch-tree-quit
     "S" #'notmuch-tree-to-search
     "C" #'notmuch-mua-new-mail ; like mu4e
     "cc" #'notmuch-mua-new-mail ; like mu4e
     "J" #'notmuch-jump-search
     "zv" #'notmuch-search-from-tree-current-query ; like mu4e-conversation
     "cr" #'notmuch-show-reply-sender ; like mu4e
     "cR" #'notmuch-show-reply
     "!" #'evil-collection-notmuch-tree-toggle-unread
     "=" #'evil-collection-notmuch-tree-toggle-flagged
     "K" #'notmuch-tag-jump
     "RET" #'notmuch-tree-show-message
     [mouse-1] #'notmuch-tree-show-message
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

(provide 'my-email-autoloads)
;;; my-email-autoloads.el ends here
