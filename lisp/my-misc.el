;;; my-misc.el -*- lexical-binding: t; -*-

(straight-use-package 'ws-butler)
(straight-use-package 'rainbow-delimiters)

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
    :ensure nil
    :defer t
    :init
    (my/run-hook-once evil-insert-state-entry-hook electric-pair-mode))

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

(use-package savehist
    :ensure nil
    :defer t
    :init
    (my/run-hook-once pre-command-hook savehist-mode)
    :config
    (setq savehist-save-minibuffer-history t
          savehist-autosave-interval nil))

(use-package recentf
    :ensure nil
    :defer t
    :init
    (my/run-hook-once pre-command-hook recentf-mode)
    :config
    (setq recentf-max-saved-items 200))

(use-package project
    :defer t
    :config
    (add-to-list 'project-switch-commands
                 '(project-dired "Dired at root")))

(my/leader
    :keymaps 'override
    :states '(visual insert motion normal)
    "u" #'universal-argument
    "h" '(:keymap help-map :which-key "help"))

(my/toggle-map
    :keymaps 'override
    :states '(motion insert normal)
    "v" #'visual-line-mode
    "t" #'toggle-truncate-lines)

(provide 'my-misc)
;;; my-misc.el ends here
