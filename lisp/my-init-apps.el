;;; my-init-apps.el -*- lexical-binding: t; -*-

(use-package eww
    :init
    ;; use google as default engine to search keyword.
    (setq eww-search-prefix "http://www.google.com/search?q="))

(provide 'my-init-apps)
;;; my-init-apps.el ends here
