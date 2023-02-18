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
          mu4e-headers-include-related nil
          mu4e-compose-format-flowed t
          mu4e-hide-index-messages t
          mu4e-completing-read-function #'completing-read
          mu4e-confirm-quit nil
          mu4e-attachment-dir (expand-file-name "Downloads" (getenv "HOME")))

    :config

    (add-hook 'mu4e-compose-mode-hook #'my/mu4e-set-mail-line-wrap)

    ;; for personal privacy, I hide my email account settings
    ;; in the following file which is a symlinked file.
    (require 'my-mail-accounts)

    ;; NOTE: it seems that when you delete a message for gmail,
    ;; those messages are really deleted.
    ;; In case you want your operation reversible,
    ;; move them to trash folder instead.

    (setq mu4e-view-actions
          '(("capture message" . mu4e-action-capture-message)
            ("view in browser" . mu4e-action-view-in-browser)
            ("xview in xwidget" . mu4e-action-view-in-xwidget)
            ("show this thread" . mu4e-action-show-thread)
            ("mailbox patch apply (git am)" . mu4e-action-git-apply-mbox)
            ("patch applying (git apply)" . mu4e-action-git-apply-patch))

          mu4e-headers-fields '((:human-date . 12)
                                (:flags . 6) ; 3 icon flags
                                (:from-or-to . 25)
                                (:subject))

          mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
          mu4e-headers-thread-orphan-prefix        '("┬>" . "┬▶ ")
          mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
          mu4e-headers-thread-first-child-prefix   '("├>" . "├▶")
          mu4e-headers-thread-child-prefix         '("├>" . "├▶")
          mu4e-headers-thread-last-child-prefix    '("└>" . "╰▶")

          mu4e-bookmarks
          '((:name "Unread messages w/o news and gmail"
             :query "flag:unread AND NOT flag:trashed AND NOT maildir:/news-milanglacier/* AND NOT maildir:/my-personal-gmail/*"
             :key ?u)
            (:name "Flagged messages"
             :query "flag:flagged"
             :key ?f)
            (:name "Unread messages w/ news"
             :query "flag:unread AND maildir:/news-milanglacier/*"
             :key ?n)
            (:name "Unread messages w/ gmail"
             :query "flag:unread AND maildir:/my-personal-gmail/*"
             :key ?g))
          )

    (plist-put (cdr (assoc :flags mu4e-header-info)) :shortname " Flags") ; default=Flgs

    ;; Due to evil, none of the marking commands work when
    ;; making a visual selection in the headers view of
    ;; mu4e. Without overriding any evil commands we may
    ;; actually want to use in and evil selection, this can be
    ;; easily fixed.
    (general-define-key
     :states 'visual
     :keymaps 'mu4e-headers-mode-map
     "*" #'mu4e-headers-mark-for-something
     "!" #'mu4e-headers-mark-for-read
     "?" #'mu4e-headers-mark-for-unread
     "u" #'mu4e-headers-mark-for-unmark)

    ;; don't map `a' to `shr-show-alt-text' which is useless.  reserve
    ;; `a' to `mu4e-view-action'
    (general-define-key :keymaps 'shr-map "a" nil)
    (general-define-key :keymaps 'shr-image-map "a" nil)


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

    :config
    (general-define-key
     :keymaps 'org-msg-edit-mode-map
     "TAB" #'org-msg-tab) ; only <tab> bound by default

    )

(provide 'my-init-email)
;;; my-init-email.el.el ends here
