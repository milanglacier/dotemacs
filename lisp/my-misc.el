;;; my-misc.el -*- lexical-binding: t; -*-

(straight-use-package 'ws-butler)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'vterm)

(use-package hideshow
    :hook (prog-mode . hs-minor-mode))

(use-package eldoc
    :init
    (setq eldoc-echo-area-use-multiline-p nil))

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
    (defun my/vterm ()
        "open vterm at project root, if no root is found, open at the default-directory"
        (interactive)
        (require 'consult)
        (let ((default-directory (or (consult--project-root)
                                     default-directory)))
            (call-interactively #'vterm)))
    (my/open-map
        :keymaps 'override
        :states '(normal insert motion)
        "t" #'my/vterm)

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

    (defun my/vterm-setup ()
        (setq-local confirm-kill-processes nil
                    hscroll-margin 0)

        (setq vterm-max-scrollback 5000))
    (add-hook 'vterm-mode-hook #'my/vterm-setup))

(use-package auto-revert
    :init
    (my/run-hook-once pre-command-hook global-auto-revert-mode))

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
