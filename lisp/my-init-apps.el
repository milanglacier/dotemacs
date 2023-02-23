;;; my-init-apps.el -*- lexical-binding: t; -*-

(straight-use-package 'elfeed)
(straight-use-package 'elfeed-org)

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

(provide 'my-init-apps)
;;; my-init-apps.el ends here
