;;; my-misc.el -*- lexical-binding: t; -*-

(straight-use-package 'ws-butler)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'vterm)
(straight-use-package 'ibuffer-vc)

;; dired
(straight-use-package 'dired-subtree)
(straight-use-package '(dired-sidebar :type git :host github :repo "milanglacier/dired-sidebar"))
(straight-use-package 'dired-rsync)
(straight-use-package 'diredfl)
(straight-use-package 'all-the-icons-dired)
(straight-use-package 'dired-git-info)

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

(use-package ibuffer-vc
    :init
    (add-hook 'ibuffer-hook #'my/ibuffer-vc-setup))

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
    (add-hook 'dired-mode-hook #'my/display-truncation-and-wrap-indicator-as-whitespace)

    ;; copied from doomemacs
    (let ((args '("-ahl" "-v" "--group-directories-first")))
        (when IS-MAC
            ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
            ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
            ;; when not using GNU ls.
            (if-let (gls (executable-find "gls"))
                    (setq insert-directory-program gls)
                ;; NOTE: BSD ls doesn't support -v or
                ;; --group-directories-first. On remote server, -v or
                ;; --group-directories-first may not supported
                ;; (e.g. BSD server). You may need to use
                ;; `file-remote-p' and `dired-mode-hook' to modify
                ;; `dired-actual-switches' on the fly.  But since I
                ;; never have a change to work with BSD server I don't
                ;; want to additionally configure for it.
                (setq args '("-ahl"))))
        (setq dired-listing-switches (string-join args " ")))

    ;; unbind SPC otherwise I cannot bind localleader key.
    (general-define-key
     :states '(normal insert motion visual)
     :keymaps 'dired-mode-map
     "SPC" nil
     "TAB" #'dired-subtree-toggle)

    (my/localleader
        :keymaps 'dired-mode-map
        :states '(normal insert motion)
        "i" #'wdired-change-to-wdired-mode
        "r" #'dired-rsync
        "h" #'dired-hide-details-mode
        "g" #'dired-git-info-mode)
    )

(use-package diredfl
    :hook (dired-mode . diredfl-mode))

(use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-sidebar
    :init
    (setq dired-sidebar-display-alist
          `((window-width . 0.25)
            (side . ,(alist-get 'dired-sidebar my/side-window-sides))
            (slot . ,(alist-get 'dired-sidebar my/side-window-slots)))
          dired-sidebar-resize-on-open nil
          dired-sidebar-window-fixed nil
          dired-sidebar-subtree-line-prefix ""
          dired-sidebar-theme 'nerd
          dired-sidebar-use-custom-modeline nil)
    (my/find-map
        :states '(normal insert motion visual)
        :keymaps 'override
        "d" #'dired-sidebar-toggle-sidebar))

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
