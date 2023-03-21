;;; my-init-apps.el -*- lexical-binding: t; -*-

(straight-use-package 'elfeed)
(straight-use-package 'elfeed-org)
(straight-use-package 'pdf-tools)

(use-package eww
    :init
    ;; use google as default engine to search keyword.
    (setq eww-search-prefix "http://www.google.com/search?q=")

    :config
    (my/localleader
        :states '(normal motion visual)
        :keymaps 'eww-mode-map
        "y" #'eww-copy-page-url
        ;; get the url of current visiting page
        "g" #'my/google-search-eww))

(use-package xwidget
    :init
    (setq my/xwidget-side-window-display
          `("\\*xwidget"
            (display-buffer-in-side-window display-buffer-reuse-window)
            (window-width . 0.33)
            (window-height . 0.5)
            (side . ,(alist-get 'xwidget-plot my/side-window-sides))
            (slot . ,(alist-get 'xwidget-plot my/side-window-slots))))

    :config
    (add-hook 'xwidget-webkit-mode-hook (my/turn-off-mode display-line-numbers-mode))
    )

(use-package elfeed
    :init
    (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
          elfeed-enclosure-default-dir (expand-file-name "closures" elfeed-db-directory))

    (my/open-map
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
     "gw" #'my:elfeed-open-entry-via-eww
     "gW" #'my:elfeed-open-entry-via-xwidget)

    (my/localleader
        :keymaps 'elfeed-show-mode-map
        :states '(normal motion visual)
        "g" #'my/google-search-eww
        "G" #'my/google-search-xwidget)


    ;; `elfeed-kill-buffer' only kills the buffer, but won't delete
    ;; the window. This is not an ideal behavior since you typically
    ;; what to hit `q' to delete the window displaying the news after
    ;; you have finished reading.
    (advice-add #'elfeed-kill-buffer :after #'my:elfeed-delete-window-after-kill-buffer)
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
                   (side . ,(alist-get 'pdf-outline my/side-window-sides))
                   (window-width . 0.3)))

    (add-hook 'pdf-outline-buffer-mode-hook #'my:font-set-small-variable-font)
    (add-hook 'pdf-outline-buffer-mode-hook (my/turn-off-mode display-line-numbers-mode))
    (add-hook 'pdf-view-mode-hook (my/setq-locally evil-normal-state-cursor nil))
    (add-hook 'pdf-view-mode-hook #'my:pdf-midnight-mode-maybe)
    (add-hook 'pdf-view-mode-hook (my/turn-off-mode display-line-numbers-mode))

    )

;; aichat integration
(use-package my-apps-autoloads
    :init
    (add-to-list 'display-buffer-alist
                 `("\\*aichat\\*"
                   (display-buffer-in-side-window display-buffer-reuse-window)
                   (window-width . 0.5)
                   (window-height 0.5)
                   (side . ,(alist-get 'aichat my/side-window-sides))
                   (slot . ,(alist-get 'aichat my/side-window-slots))))

    (general-create-definer my/chatgpt-map
        :prefix "SPC c"
        :non-normal-prefix "M-SPC c"
        :prefix-map 'my/chatgpt-map)

    (my/chatgpt-map
        :keymaps 'override
        :states '(normal insert motion visual)
        "s" #'my~aichat-start
        "r" #'my~aichat-send-region-operator))


(provide 'my-init-apps)
;;; my-init-apps.el ends here
