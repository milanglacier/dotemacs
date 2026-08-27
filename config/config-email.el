;;; config-email.el -*- lexical-binding: t; -*-

(straight-use-package '(notmuch :type built-in))
(straight-use-package 'org-msg)
(straight-use-package 'consult-notmuch)
(straight-use-package 'ol-notmuch)

;; NOTE: It is recommended to use the notmuch emacs library shipped
;; with the notmuch binary. If notmuch is installed by a proper
;; package manager, then the directory should be put into one of the
;; default location in the `load-path'.

(use-package consult-notmuch
    :init
    (mg-open-map
        :states '(normal insert motion visual)
        :keymaps 'override
        "M" #'consult-notmuch))

(use-package notmuch
    :commands notmuch
    :init
    (mg-open-map
        :states '(normal insert motion visual)
        :keymaps 'override
        "m" #'notmuch)
    (setq mail-user-agent 'notmuch-user-agent
          message-confirm-send nil
          message-kill-buffer-on-exit t
          message-mail-user-agent t
          fill-flowed-encode-column 72
          sendmail-program (executable-find "msmtp")
          send-mail-function #'smtpmail-send-it
          message-send-mail-function #'message-send-mail-with-sendmail
          message-sendmail-f-is-evil t
          message-sendmail-extra-arguments '("--read-envelope-from")
          message-kill-buffer-on-exit t ; close after sending
          smtpmail-stream-type 'starttls
          )

    :config
    ;; modified from evil-collection-notmuch, only remove keybindings that I don't need.
    (mg--notmuch-setup)

    ;; this file loads my user name and email account, a.k.a set the
    ;; variables `notmuch-identities' something similar to this

    ;; (setq ;; notmuch-identities '("User Name <user@domain.com"
    ;;                               "User Name 2 <user2@domain.com>"))
    (require 'config-mail-accounts)

    ;; Give email buffers a sane name so they can be targeted via
    ;; `display-buffer-alist'.
    (advice-add #'notmuch-search-show-thread :around #'mg--notmuch-search-show-thread)

    ;; Ensure proper truncation of all fields in notmuch search
    ;; results. Notmuch only truncated long strings for the "authors"
    ;; field. This advice extends truncation to all fields, including
    ;; "subjects". It guarantees that format strings like "%-80s" will
    ;; limit output to 80 characters
    (advice-add #'notmuch-search-insert-field :around #'mg--notmuch-ensure-field-truncation)

    ;; always latest messages display at top
    (setq-default notmuch-search-oldest-first nil)

    ;; those action-* tags will be executed in a pre-new hook script
    ;; which moves the messages to the corrsponding folders.
    (setq notmuch-show-logo nil
          ;; even if there are no messages in a bookmark, still show it in `notmuch-hello-mode'
          notmuch-show-empty-saved-searches t
          ;; prefer html over plain text when both formats are available
          notmuch-multipart/alternative-discouraged '("text/plain" "text/html")
          notmuch-fcc-dirs nil ; don't save sent messages. The remove server will do this for me
          notmuch-archive-tags '("+action-archive")
          notmuch-draft-tags '("+drafts")
          mg-notmuch-deleted-tags "action-delete"
          ;; call `notmuch-tag-jump', in the popup menu, press this key will reverse the tagging operation
          notmuch-tag-jump-reverse-key "K"
          notmuch-always-prompt-for-sender t ;; prompts for sender when forwarding message
          notmuch-tagging-keys '(("a" notmuch-archive-tags "Archive")
                                 ("u" notmuch-show-mark-read-tags "Mark read")
                                 ("f" ("+flagged") "Flag")
                                 ("s" ("+action-spam") "Mark as spam")
                                 ("d" ("+action-delete") "Delete"))
          notmuch-saved-searches
          '((:name "unread important messages"
             :query "tag:unread and not folder:/gmail/ and not folder:/news-milanglacier/"
             :key "u")
            (:name "unread gmail"
             :query "tag:unread and folder:/gmail/"
             :key "g")
            (:name "unread news"
             :query "tag:unread and folder:/news-milanglacier/"
             :key "n")
            (:name "flagged" :query "tag:flagged" :key "f")
            (:name "archived" :query "tag:archive" :key "a")
            (:name "sent" :query "tag:sent" :key "S")
            )
          notmuch-search-result-format
          '(("date" . "%12s ")
            ("count" . "%-7s ")
            ("authors" . "%-25s  ")
            ("subject" . "%-90s  ")
            ("tags" . "(%s)"))
          notmuch-tag-formats
          '(("unread" (propertize "" 'face 'notmuch-tag-unread))
            ("flagged" (propertize "" 'face 'notmuch-tag-flagged))
            ("archive" (propertize "" 'face 'notmuch-tag-flagged))
            ("deleted" "")
            ("spam" "󰉚")
            ("inbox" "")
            ("action-archive" "[]")
            ("action-delete" "[]")
            ("action-spam" "[󰉚]")
            ("action-inbox" "[]")
            ("sent" "󰗍")
            ("attachment" "󰁦")
            ("emacs-org" "")
            ("emacs.*" "")
            ("list" "")
            ("replied" "")
            ("signed" "󱧃")
            ("misc-list" "")
            ("R" "")
            ("python" "")
            ("neovim" "")
            ("vim" "")
            ("new" "󰇮"))
          notmuch-tag-deleted-formats
          '((".*" (notmuch-apply-face (concat "-" tag) `notmuch-tag-deleted)))
          notmuch-tag-added-formats
          '((".*" (notmuch-apply-face (concat "+" tag) 'notmuch-tag-added)))
          )

    (mg-setq-on-hook notmuch-hello-mode-hook compile-command "notmuch new; mbsync -a; notmuch new;")
    ;; run compile directly without prompting the command in minibuffer.
    ;; HOLD: decide to use minibuffer prompt now.
    ;; (add-hook 'notmuch-hello-mode-hook (mg-setq-locally compilation-read-command nil))

    (org-msg-mode)

    )


;; copied from doomemacs mu4e module
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

    (add-to-list 'mg-function-predicate-blocklist
                 '(eglot-ensure (derived-mode-p 'org-msg-edit-mode)))

    :config
    (add-hook 'org-msg-edit-mode-hook (mg-turn-off-mode diff-hl-mode))
    (mg-setq-on-hook org-msg-edit-mode-hook company-backends
                     `(notmuch-company ,@company-backends))
    )

(provide 'config-email)
;;; config-email.el.el ends here
