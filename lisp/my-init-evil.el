;;; my-init-evil.el -*- lexical-binding: t; -*-

(straight-use-package 'evil)
(straight-use-package 'evil-goggles)
(straight-use-package 'evil-escape)
(straight-use-package 'better-jumper)
(straight-use-package 'evil-anzu)

(use-package evil
  :hook (after-init . evil-mode)

  :init
  (setq evil-want-C-i-jump nil
        evil-want-C-u-delete nil
        evil-want-C-u-scroll nil
        evil-want-Y-yank-to-eol t
        evil-want-C-u-delete t
        evil-want-C-w-delete t
        evil-want-abbrev-expand-on-insert-exit t
        evil-visual-update-x-selection-p nil
        evil-mode-line-format nil)

  :config
  (evil-goggles-mode)
  (evil-escape-mode)
  (evil-select-search-module 'evil-search-module 'evil-search)

  )

(use-package evil-goggles
  :init
  (setq evil-goggles-duration 0.4
        evil-goggles-pulse nil
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil))

(use-package evil-escape
  :init
  (setq evil-escape-key-sequence "jk"))

(use-package! evil-anzu
  :hook (evil-mode . global-anzu-mode))

(provide 'my-init-evil)
;;; my-init-evil.el ends here
