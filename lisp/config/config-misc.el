;;; config-misc.el -*- lexical-binding: t; -*-

(straight-use-package 'ws-butler)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'vterm)

;; ibuffer
(straight-use-package 'ibuffer-vc)
(straight-use-package 'nerd-icons-ibuffer)

;; dired
(straight-use-package 'dired-sidebar)
(straight-use-package 'dired-rsync)
(straight-use-package 'diredfl)
(straight-use-package 'dired-git-info)
(straight-use-package 'nerd-icons-dired)

(straight-use-package 'editorconfig)

(use-package hideshow
    :hook (prog-mode . hs-minor-mode))

;; automatically remove trailing whitespaces
(use-package ws-butler
    :init
    (mg-run-hook-once pre-command-hook ws-butler-global-mode)

    :config
    (setq ws-butler-keep-whitespace-before-point nil))

(use-package elec-pair
    :init
    (mg-run-hook-once evil-insert-state-entry-hook electric-pair-mode)

    :config
    ;; more conservative on whether should also insert ) when typing
    ;; (, for example, prevent from inserting ) when point is on a
    ;; word.
    (setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit))

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

(use-package savehist
    :init
    (mg-run-hook-once pre-command-hook savehist-mode)
    :config
    (setq savehist-save-minibuffer-history t
          savehist-autosave-interval nil))

(use-package recentf
    :init
    (mg-run-hook-once pre-command-hook recentf-mode)
    :config
    (setq recentf-max-saved-items 200))

(use-package project
    :init
    ;; submodule should not be considered as part of parent project
    (setq project-vc-merge-submodules nil)

    :config
    (add-to-list 'project-switch-commands
                 '(project-dired "Dired at root"))
    (add-to-list 'project-switch-commands
                 '(vterm "vterm"))
    (add-to-list 'project-switch-commands
                 '(mg-project-magit "magit"))

    (remove-hook 'project-switch-commands '(project-vc-dir "VC-Dir"))

    (general-define-key
     :keymaps 'project-prefix-map
     "v" #'vterm
     "m" #'mg-project-magit)

    )

(use-package vterm
    :init
    (mg-open-map
        :keymaps 'override
        :states '(normal insert motion)
        "t" #'vterm)

    :config

    (advice-add #'vterm :around #'call-command-at-project-root)

    (setq vterm-max-scrollback 5000)

    (add-to-list 'display-buffer-alist
                 `("\\*vterm\\*"
                   (display-buffer-in-side-window)
                   (window-height . 0.4)
                   (window-width .0.5)
                   (slot . ,(alist-get 'vterm mg-side-window-slots))))

    (general-define-key
     :keymaps 'vterm-mode-map
     "C-c <escape>" #'vterm-send-escape)

    (add-hook 'vterm-mode-hook (mg-setq-locally confirm-kill-processes nil))
    ;; From doomemacs: Prevent premature horizontal scrolling
    (add-hook 'vterm-mode-hook (mg-setq-locally hscroll-margin 0))
    )

(use-package auto-revert
    :init
    (mg-run-hook-once pre-command-hook global-auto-revert-mode))

(use-package ibuffer-vc
    :init
    (add-hook 'ibuffer-hook #'mg-ibuffer-vc-setup))

(use-package nerd-icons-ibuffer
    :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package dired
    :init
    (setq dired-dwim-target t
          dired-hide-details-hide-symlink-targets nil
          ;; don't prompt to revert, just do it
          dired-auto-revert-buffer #'dired-buffer-stale-p
          ;; Always copy/delete recursively
          dired-recursive-copies 'always
          dired-recursive-deletes 'top
          ;; Ask whether destination dirs should get created when copying/removing files.
          dired-create-destination-dirs 'ask
          ;; Disable the prompt about whether I want to kill the Dired buffer for a
          ;; deleted directory. Of course I do!
          dired-clean-confirm-killing-deleted-buffers nil
          ;; Screens are larger nowadays, we can afford slightly larger thumbnails
          image-dired-thumb-size 150)

    :config
    (add-hook 'dired-mode-hook #'mg-display-truncation-and-wrap-indicator-as-whitespace)

    ;; adapted from doomemacs
    (if IS-MAC
            (if (executable-find "gls")
                    ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
                    ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
                    ;; when not using GNU ls.
                    (progn
                        (setq insert-directory-program "gls")
                        (setq dired-listing-switches "-ahl -v --group-directories-first"))
                (setq dired-listing-switches "-ahl"))
        (setq dired-listing-switches "-ahl -v --group-directories-first"))
    ;; NOTE: BSD ls doesn't support -v or
    ;; --group-directories-first. On remote server, -v or
    ;; --group-directories-first may not supported
    ;; (e.g. BSD server). You may need to use
    ;; `file-remote-p' and `dired-mode-hook' to modify
    ;; `dired-actual-switches' on the fly.  But since I
    ;; never have a change to work with BSD server I don't
    ;; want to additionally configure for it.

    (general-define-key
     :states '(normal insert motion visual)
     :keymaps 'dired-mode-map
     "TAB" #'dired-subtree-toggle
     "g TAB" #'mg-dired-find-file-other-tab)

    (mg-localleader
        :keymaps 'dired-mode-map
        :states '(normal insert motion)
        "i" #'wdired-change-to-wdired-mode
        "r" #'dired-rsync
        "h" #'dired-hide-details-mode
        "g" #'dired-git-info-mode)
    )

(use-package diredfl
    :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
    :hook ((dired-mode . nerd-icons-dired-mode)
           (nerd-icons-dired-mode . mg--dired-subtree-toggle-nerd-icons)))

(use-package dired-sidebar
    :init
    (setq dired-sidebar-display-alist
          `((window-width . 0.25)
            (side . ,(alist-get 'dired-sidebar mg-side-window-sides))
            (slot . ,(alist-get 'dired-sidebar mg-side-window-slots)))
          dired-sidebar-resize-on-open nil
          dired-sidebar-window-fixed nil
          dired-sidebar-subtree-line-prefix ""
          dired-sidebar-theme 'nerd
          dired-sidebar-use-custom-modeline nil)
    (mg-find-map
        :states '(normal insert motion visual)
        :keymaps 'override
        "d" #'dired-sidebar-toggle-sidebar)

    :config
    (add-hook 'dired-sidebar-mode-hook #'mg--font-set-small-mono-font))

(use-package editorconfig
    :init
    (mg-run-hook-once pre-command-hook editorconfig-mode)

    :config
    (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)

    (add-to-list 'editorconfig-exclude-regexps
                 "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'"))

(mg-leader
    :keymaps 'override
    :states '(visual insert motion normal)
    "u" #'universal-argument
    "-" #'negative-argument
    "h" '(:keymap help-map :which-key "help"))

(mg-toggle-map
    :keymaps 'override
    :states '(motion insert normal)
    "v" #'visual-line-mode
    "t" #'toggle-truncate-lines)

(provide 'config-misc)
;;; config-misc.el ends here
