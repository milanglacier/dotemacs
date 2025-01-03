;;; config-vcs.el -*- lexical-binding: t; -*-

(straight-use-package 'magit)
(straight-use-package 'diff-hl)
(straight-use-package 'hl-todo)

(general-create-definer mg-git-map
    :prefix "SPC g"
    :non-normal-prefix "M-SPC g"
    :prefix-map 'mg-git-map)

(use-package magit
    :init
    (mg-git-map
        :states '(normal insert visual insert)
        :keymaps 'override
        "" '(:ignore t :which-key "git")
        "g" #'magit
        "b" #'magit-blame
        "a" #'magit-file-dispatch
        "A" #'magit-dispatch)

    (setq magit-diff-refine-hunk t ; show granular diffs in selected hunk.
          ;; Don't autosave repo buffers. This is too magical, and
          ;; saving can trigger a bunch of unwanted side-effects, like
          ;; save hooks and formatters. Trust the user to know what
          ;; they're doing.
          magit-save-repository-buffers nil
          magit-define-global-key-bindings nil)

    :config
    (add-to-list 'display-buffer-alist
                 '("magit:" ;; the main magit dashboard
                   ;; If a magit window exists, use it instead of
                   ;; creating a new one.  If no magit window exists,
                   ;; create a new tab.  When a magit tab already
                   ;; exists, display the magit buffer in that tab's
                   ;; window, replacing any existing content.
                   (display-buffer-in-tab display-buffer-same-window)
                   (tab-name . mg--get-tab-name)))

    (general-define-key
     :states '(normal motion)
     :keymaps 'magit-status-mode-map
     "gt" #'tab-bar-switch-to-next-tab
     "gT" #'tab-bar-switch-to-prev-tab
     ;; emulate C-g
     "<escape>" #'transient-quit-one))

(use-package diff-hl
    :hook ((prog-mode . diff-hl-mode)
           (conf-mode . diff-hl-mode)
           (text-mode . diff-hl-mode))

    :init
    (mg-git-map
        :states '(normal insert visual insert)
        :keymaps 'override
        "r" #'diff-hl-revert-hunk
        "s" #'diff-hl-stage-current-hunk
        ;; preview
        "p" #'diff-hl-show-hunk)

    :config
    (setq diff-hl-show-hunk-inline-popup-smart-lines nil
          diff-hl-show-hunk-inline-popup-hide-hunk t
          diff-hl-show-staged-changes nil)

    (general-define-key
     :states '(normal visual motion)
     "]h" #'diff-hl-next-hunk
     "[h" #'diff-hl-previous-hunk)

    (unless (display-graphic-p)
        (diff-hl-margin-mode))

    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

    )

(use-package hl-todo
    :hook ((prog-mode . hl-todo-mode)
           (conf-mode . hl-todo-mode))
    :init
    (setq hl-todo-highlight-punctuation ":")
    (mg-git-map
        :states '(normal insert visual insert)
        :keymaps 'override
        "t" #'mg-project-todos)
    )

(provide 'config-vcs)
;;; config-vcs.el ends here
