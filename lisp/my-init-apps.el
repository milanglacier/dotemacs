;;; my-init-apps.el -*- lexical-binding: t; -*-

(use-package eww
    :init
    ;; use google as default engine to search keyword.
    (setq eww-search-prefix "http://www.google.com/search?q="))

(use-package xwidget
    :init
    (setq my/xwidget-side-window-display
          `("\\*xwidget"
            (display-buffer-in-side-window display-buffer-reuse-window)
            (window-width . 0.33)
            (window-height . 0.5)
            (side . ,(alist-get 'xwidget-plot my/side-window-sides))
            (slot . ,(alist-get 'xwidget-plot my/side-window-slots))))
    )

(provide 'my-init-apps)
;;; my-init-apps.el ends here
