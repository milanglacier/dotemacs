;;; config-apps.el -*- lexical-binding: t; -*-

(straight-use-package 'elfeed)
(straight-use-package 'elfeed-org)
(straight-use-package 'pdf-tools)

(use-package eww
    :init
    ;; use google as default engine to search keyword.
    (setq eww-search-prefix "http://www.google.com/search?q=")

    :config
    (mg-localleader
        :states '(normal motion visual)
        :keymaps 'eww-mode-map
        "y" #'eww-copy-page-url
        ;; get the url of current visiting page
        "g" #'mg-google-search-eww))

(use-package xwidget
    :init
    (setq mg-xwidget-side-window-display
          `("\\*xwidget"
            (display-buffer-in-side-window display-buffer-reuse-window)
            (window-width . 0.33)
            (window-height . 0.5)
            (side . ,(alist-get 'xwidget-plot mg-side-window-sides))
            (slot . ,(alist-get 'xwidget-plot mg-side-window-slots))))
    )

(use-package elfeed
    :init
    (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
          elfeed-enclosure-default-dir (expand-file-name "closures" elfeed-db-directory))

    (mg-open-map
        :states '(normal insert motion visual)
        :keymaps 'override
        "w" #'elfeed)

    :config
    (setq elfeed-search-filter "@2-week-ago "
          elfeed-show-entry-switch #'pop-to-buffer
          shr-max-image-proportion 0.7)

    (add-to-list 'display-buffer-alist
                 '("\\*elfeed-entry"
                   (display-buffer-below-selected)
                   (window-height . 0.85)))

    (general-define-key
     :states 'normal
     :keymaps 'elfeed-show-mode-map
     "gw" #'mg--elfeed-open-entry-via-eww
     "gW" #'mg--elfeed-open-entry-via-xwidget)

    (mg-localleader
        :keymaps 'elfeed-show-mode-map
        :states '(normal motion visual)
        "g" #'mg-google-search-eww
        "G" #'mg-google-search-xwidget)


    ;; `elfeed-kill-buffer' only kills the buffer, but won't delete
    ;; the window. This is not an ideal behavior since you typically
    ;; what to hit `q' to delete the window displaying the news after
    ;; you have finished reading.
    (advice-add #'elfeed-kill-buffer :after #'mg--elfeed-delete-window-after-kill-buffer)
    )

(use-package elfeed-org
    :after elfeed
    :demand t
    :init
    (setq rmh-elfeed-org-files `(,(expand-file-name "elfeed.org" org-directory)))

    :config
    (elfeed-org))

(use-package pdf-tools
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :init
    (setq pdf-view-display-size 'fit-page
          ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
          pdf-view-use-scaling t
          pdf-view-use-imagemagick nil
          pdf-view-continuous nil)

    :config
    (evil-collection-pdf-setup)

    (add-to-list 'display-buffer-alist
                 `("\\*[oO]utline.*pdf\\*"
                   (display-buffer-in-side-window display-buffer-reuse-window)
                   (side . ,(alist-get 'pdf-outline mg-side-window-sides))
                   (window-width . 0.3)))

    (add-hook 'pdf-outline-buffer-mode-hook #'mg--font-set-small-variable-font)
    (add-hook 'pdf-view-mode-hook (mg-setq-locally evil-normal-state-cursor nil))
    (add-hook 'pdf-view-mode-hook #'mg--pdf-midnight-mode-maybe)

    )

;; aichat (a LLM based chat REPL) integration
(add-to-list 'display-buffer-alist
             `("\\*aichat\\*"
               (display-buffer-in-side-window display-buffer-reuse-window)
               (window-width . 0.5)
               (window-height 0.5)
               (side . ,(alist-get 'aichat mg-side-window-sides))
               (slot . ,(alist-get 'aichat mg-side-window-slots))))

(general-create-definer mg-chat-map
    :prefix "SPC c"
    :non-normal-prefix "M-SPC c"
    :prefix-map 'mg-chat-map)

(mg-chat-map
    :keymaps 'override
    :states '(normal insert motion visual)
    "s" #'vtr~aichat-start
    "e" #'vtr~aichat-send-string
    "r" #'vtr~aichat-send-region-operator
    "h" #'vtr~aichat-hide-window)


;; aider (a llm based code assistant) integration
(add-to-list 'display-buffer-alist
             `("\\*aider\\*"
               (display-buffer-in-tab)
               (tab-name . mg--get-tab-name)))

(general-create-definer mg-aider-map
    :prefix "SPC a"
    :non-normal-prefix "M-SPC a"
    :prefix-map 'mg-aider-map)

(mg-aider-map
    :keymaps 'override
    :states '(normal insert motion visual)
    "s" #'vtr~aider-start
    "r" #'vtr~aider-send-region-operator
    "h" #'vtr~aider-hide-window
    "e" #'vtr-aider-prompt
    "g" #'vtr-aider-set-prefix
    "G" #'vtr-aider-remove-prefix
    "y" #'vtr-aider-yes
    "n" #'vtr-aider-no
    "a" #'vtr-aider-abort
    "q" #'vtr-aider-exit
    "ma" #'vtr-aider-ask-mode
    "mA" #'vtr-aider-arch-mode
    "mc" #'vtr-aider-code-mode)

(provide 'config-apps)
;;; config-apps.el ends here
