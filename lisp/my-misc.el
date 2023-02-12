;;; my-misc.el -*- lexical-binding: t; -*-

(straight-use-package 'ws-butler)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'vterm)
(straight-use-package 'ibuffer-vc)

(use-package hideshow
    :hook (prog-mode . hs-minor-mode))

;; automatically remove trailing whitespaces
(use-package ws-butler
    :init
    (my/run-hook-once pre-command-hook ws-butler-global-mode)

    :config
    (setq ws-butler-keep-whitespace-before-point nil))

(use-package elec-pair
    :init
    (my/run-hook-once evil-insert-state-entry-hook electric-pair-mode))

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

(use-package savehist
    :init
    (my/run-hook-once pre-command-hook savehist-mode)
    :config
    (setq savehist-save-minibuffer-history t
          savehist-autosave-interval nil))

(use-package recentf
    :init
    (my/run-hook-once pre-command-hook recentf-mode)
    :config
    (setq recentf-max-saved-items 200))

(use-package project
    :config
    (add-to-list 'project-switch-commands
                 '(project-dired "Dired at root")))

(use-package vterm
    :init
    (my/open-map
        :keymaps 'override
        :states '(normal insert motion)
        "t" #'my/vterm)
    (add-hook 'vter-mode-hook #'my/vterm-setup)

    :config

    (add-to-list 'display-buffer-alist
                 `("\\*vterm\\*"
                   (display-buffer-in-side-window)
                   (window-height . 0.4)
                   (window-width .0.5)
                   ;; if there are multiple window, prefer the window to the right window
                   (slot . ,(alist-get 'vterm my/side-window-slots))))

    (general-define-key
     :keymaps 'vterm-mode-map
     "C-c <escape>" #'vterm-send-escape)

    )

(use-package auto-revert
    :init
    (my/run-hook-once pre-command-hook global-auto-revert-mode))

(use-package ibuffer
    :init
    (add-hook 'ibuffer-hook #'my/ibuffer-vc-setup))

(my/leader
    :keymaps 'override
    :states '(visual insert motion normal)
    "u" #'universal-argument
    "-" #'negative-argument
    "h" '(:keymap help-map :which-key "help"))

(my/toggle-map
    :keymaps 'override
    :states '(motion insert normal)
    "v" #'visual-line-mode
    "t" #'toggle-truncate-lines)

(provide 'my-misc)
;;; my-misc.el ends here
