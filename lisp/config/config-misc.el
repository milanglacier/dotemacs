;;; config-misc.el -*- lexical-binding: t; -*-

;; NOTE: Avoid fetching ws-butler from Savannah NonGNU ELPA upstream
;; due to unstable network connections to Savannah. Instead, use the
;; GitHub mirror. See radian-software/straight.el#1189.
(straight-use-package
 '(ws-butler :type git
             :repo "https://github.com/emacsmirror/nongnu_elpa"
             :branch "elpa/ws-butler"
             :depth (full single-branch)
             :local-repo "ws-butler"))

(straight-use-package 'rainbow-delimiters)
(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;; ibuffer
(straight-use-package 'ibuffer-vc)
(straight-use-package 'nerd-icons-ibuffer)

;; dired
(straight-use-package 'dired-sidebar)
(straight-use-package 'dired-rsync)
(straight-use-package 'diredfl)
(straight-use-package 'dired-git-info)
(straight-use-package 'nerd-icons-dired)

(straight-use-package '(editorconfig :type built-in))

(use-package hideshow
    :hook (prog-mode . hs-minor-mode))

;; automatically remove trailing whitespaces
(use-package ws-butler
    :init
    (mg-run-hook-once find-file-hook ws-butler-global-mode)

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
                 '(eat "eat"))
    (add-to-list 'project-switch-commands
                 '(mg-project-magit "magit"))

    (remove-hook 'project-switch-commands '(project-vc-dir "VC-Dir"))

    (general-define-key
     :keymaps 'project-prefix-map
     "v" #'eat
     "m" #'mg-project-magit)

    )

;; NOTE: If the Eat terminal isn't functioning correctly, this might
;; be a terminfo issue. The terminfo database provided with Eat might
;; not be compatible with your system. To resolve this, run
;; `eat-compile-terminfo' and restart Eat.

(use-package eat
    :init
    (mg-open-map
        :keymaps 'override
        :states '(normal insert motion)
        "t" #'eat)

    :config
    (setq eat-kill-buffer-on-exit t)

    (general-define-key
     :keymaps 'eat-semi-char-mode-map
     "C-c <escape>" #'eat-self-input)

    (advice-add #'eat :around #'call-command-at-project-root)

    (add-to-list 'display-buffer-alist
                 `("\\*eat\\*"
                   (display-buffer-in-side-window)
                   (window-height . 0.4)
                   (window-width .0.5)
                   (slot . ,(alist-get 'eat mg-side-window-slots))))

    (add-hook 'eat-mode-hook (mg-setq-locally hscroll-margin 0)))

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

    (cond (IS-MAC (setq dired-listing-switches "-alh"))
          (IS-LINUX (setq dired-listing-switches "-alh -v --group-directories-first")))
    ;; NOTE: BSD ls doesn't support -v or
    ;; --group-directories-first. On a BSD Server remote server.  You
    ;; may need to use `file-remote-p' and `dired-mode-hook' to modify
    ;; `dired-actual-switches' with remote detection.  But since I
    ;; never have a change to work with BSD server I don't want to
    ;; additionally configure for it.

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
    (mg-advise-at-once find-file editorconfig-mode :before)
    ;; don't let editorconfig modify `lisp-indent-offset'
    (setq editorconfig-lisp-use-default-indent t))

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
