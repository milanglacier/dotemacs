;;; my-init-email.el -*- lexical-binding: t; -*-

(straight-use-package 'org-msg)

;; NOTE: It is recommended to use the mu4e.el shipped with the
;; mu4e binary. If mu4e is installed by a proper package
;; manager, then the directory of mu4e.el should be put into one of
;; the default location in the `load-path'.

(use-package mu4e
    :commands mu4e
    :init
    (my/open-map
        :states '(normal insert motion visual)
        :keymaps 'override
        "m" #'mu4e)
    (setq sendmail-program (executable-find "msmtp")
          send-mail-function #'smtpmail-send-it
          message-send-mail-function #'smtpmail-send-it
          message-sendmail-f-is-evil t
          message-sendmail-extra-arguments '("--read-envelope-from")
          message-send-mail-function #'message-send-mail-with-sendmail
          message-kill-buffer-on-exit t ; close after sending
          smtpmail-stream-type 'starttls
          fill-flowed-encode-column 72

          mail-user-agent 'mu4e-user-agent
          message-mail-user-agent 'mu4e-user-agent

          ;; start with the first (default) context;
          mu4e-context-policy 'pick-first
          ;; compose with the current context, or ask
          mu4e-compose-context-policy 'ask-if-none
          mu4e-get-mail-command "mbsync -a"
          mu4e-change-filenames-when-moving t
          mu4e-sent-messages-behavior 'delete
          ;; outlook automatically save the sent messages, should directly delete it
          mu4e-compose-format-flowed t
          mu4e-maildir-initial-input "" ;; the default is "/", which causes some dirs cannot be completed
          mu4e-hide-index-messages t
          mu4e-completing-read-function #'completing-read
          mu4e-confirm-quit nil
          mu4e-attachment-dir (expand-file-name "Downloads/" (getenv "HOME")))

    :config

    (my~mu4e-thread-folding-mode)

    (add-hook 'mu4e-compose-mode-hook (my/setq-locally fill-column 72))
    (add-hook 'mu4e-headers-mode-hook (my/turn-off-mode display-line-numbers-mode))
    (add-hook 'mu4e-main-mode-hook (my/turn-off-mode display-line-numbers-mode))
    (add-hook 'mu4e-view-mode-hook (my/turn-off-mode display-line-numbers-mode))

    ;; To keep personal privacy, I hide my email account settings in
    ;; the `my-mail-accounts.el' which is a symlinked file.  This file
    ;; looks like:

    ;; (setq mu4e-contexts
    ;;       `(,(make-mu4e-context
    ;;           :name "account 1"
    ;;           :enter-func (my/mu4e-enter-func 1) ;; compose html email
    ;;           :leave-func (my/mu4e-leave-func)
    ;;           :match-func (my/mu4e-match-func "/my-mail-1")
    ;;           :vars '((mu4e-sent-folder . "/my-mail-1/Sent")
    ;;                   (mu4e-drafts-folder . "/my-mail-1/Drafts")
    ;;                   (mu4e-trash-folder . "/my-mail-1/Deleted")
    ;;                   (mu4e-refile-folder . "/my-mail-1/Archive")
    ;;                   (user-full-name . "my name 1")
    ;;                   (user-mail-address . "my account 1")
    ;;                   (smtpmail-smtp-user . "my account 1")))
    ;;         ,(make-mu4e-context
    ;;           :name "account 2"
    ;;           :enter-func (my/mu4e-enter-func -1) ;; compose plain text email
    ;;           :leave-func (my/mu4e-leave-func)
    ;;           :match-func (my/mu4e-match-func "/my-mail-2")
    ;;           :vars '((mu4e-sent-folder . "/my-mail-2/Sent Mail")
    ;;                   (mu4e-drafts-folder . "/my-mail-2/Drafts")
    ;;                   (mu4e-trash-folder . "/my-mail-2/Trash")
    ;;                   (mu4e-refile-folder . "/my-mail-2/My-Archive")
    ;;                   (user-full-name . "my name 2")
    ;;                   (user-mail-address . "my mail 2")
    ;;                   (smtpmail-smtp-user . "my mail 2")))))

    (require 'my-mail-accounts)

    ;; NOTE: it seems that when you delete a message for gmail,
    ;; those messages are really deleted.
    ;; In case you want your operation reversible,
    ;; move them to trash folder instead.

    (setq mu4e-view-actions
          '(("capture message" . mu4e-action-capture-message)
            ("view in browser" . mu4e-action-view-in-browser)
            ("xview in xwidget" . my:mu4e-action-view-in-xwidget)
            ("eview in eww" . my:mu4e-open-link-via-eww)
            ("show this thread" . mu4e-action-show-thread)
            ("mailbox patch apply (git am)" . mu4e-action-git-apply-mbox)
            ("patch applying (git apply)" . mu4e-action-git-apply-patch))

          mu4e-headers-fields '((:human-date . 12)
                                (:flags . 6)
                                (:from-or-to . 25)
                                (:subject))

          mu4e-headers-thread-single-orphan-prefix '("─▶ " . "─>")
          mu4e-headers-thread-orphan-prefix        '("┬▶ " . "┬>")
          mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
          mu4e-headers-thread-first-child-prefix   '("├▶ " . "├>")
          mu4e-headers-thread-child-prefix         '("├▶ " . "├>")
          mu4e-headers-thread-last-child-prefix    '("╰▶ " . "└>")

          mu4e-bookmarks
          '((:name "Flagged messages"
             :query "flag:flagged"
             :key ?f)
            (:name "Unread important messages"
             :query "flag:unread AND NOT flag:trashed AND NOT maildir:/news-milanglacier/* AND NOT maildir:/my-personal-gmail/*"
             :key ?u)
            (:name "Unread news"
             :query "flag:unread AND maildir:/news-milanglacier/*"
             :key ?n)
            (:name "Unread gmail messages"
             :query "flag:unread AND maildir:/my-personal-gmail/*"
             :key ?g))
          )

    (plist-put (cdr (assoc :flags mu4e-header-info)) :shortname " Flags") ; default=Flgs

    (general-define-key
     :states '(normal motion)
     :keymaps 'mu4e-headers-mode-map
     "] t" #'my~mu4e-thread-forward-start
     "] T" #'my~mu4e-thread-forward-end
     "[ t" #'my~mu4e-thread-backward-start
     "[ T" #'my~mu4e-thread-backward-end)

    (general-define-key
     :states '(normal motion)
     :keymaps 'mu4e-view-mode-map
     "M-RET" #'mu4e--view-browse-url-from-binding)

    (general-define-key
     :states '(normal motion)
     :keymaps 'mu4e-headers-mode-map
     "za" #'my~mu4e-toggle-thread-folding-at-point
     "zc" #'my~mu4e-fold-thread-at-point
     "zo" #'my~mu4e-unfold-thread-at-point
     "zm" #'my~mu4e-fold-all-threads
     "zr" #'my~mu4e-unfold-all-threads)

    (general-define-key
     :states '(normal motion)
     :keymaps 'mu4e-view-mode-map
     "] t" #'my~mu4e-view-thread-forward
     "[ t" #'my~mu4e-view-thread-backward)

    ;; don't map `a' to `shr-show-alt-text' which is useless.  reserve
    ;; `a' to `mu4e-view-action'
    (general-define-key :keymaps 'shr-map "a" nil)
    (general-define-key :keymaps 'shr-image-map "a" nil)

    (add-hook 'mu4e-compose-mode-hook (my/turn-off-mode diff-hl-mode))
    )

(use-package org-msg
    :init
    (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil tex:dvipng"
          org-msg-startup "hidestars indent inlineimages"
          org-msg-greeting-name-limit 3
          org-msg-default-alternatives '((new utf-8 html)
                                         (reply-to-text utf-8)
                                         (reply-to-html utf-8 html))
          org-msg-convert-citation t
          ;; The default attachment matcher gives too many false
          ;; positives, it's better to be more conservative. See
          ;; https://regex101.com/r/EtaiSP/4.
          org-msg-attached-file-reference
          "see[ \t\n]\\(?:the[ \t\n]\\)?\\(?:\\w+[ \t\n]\\)\\{0,3\\}\\(?:attached\\|enclosed\\)\\|\
(\\(?:attached\\|enclosed\\))\\|\
\\(?:attached\\|enclosed\\)[ \t\n]\\(?:for\\|is\\)[ \t\n]")

    (add-to-list 'my$function-predicate-blocklist
                 '(eglot-ensure (derived-mode-p 'org-msg-edit-mode)))

    :config
    (add-hook 'org-msg-edit-mode-hook (my/turn-off-mode diff-hl-mode)))

(provide 'my-init-email)
;;; my-init-email.el.el ends here
