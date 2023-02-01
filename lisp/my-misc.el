;;; my-misc.el -*- lexical-binding: t; -*-

(straight-use-package 'ws-butler)

(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode))

(use-package eldoc
  :ensure nil
  :defer t
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

;; automatically remove trailing whitespaces
(use-package ws-butler
  :hook (after-init . ws-butler-global-mode)
  :config
  (setq ws-butler-keep-whitespace-before-point nil))

(use-package elec-pair
  :defer t
  :init
  (my/run-hook-once evil-insert-state-entry-hook electric-pair-mode))

(provide 'my-misc)
;;; my-misc.el ends here
