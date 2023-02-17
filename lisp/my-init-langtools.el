;;; my-init-langtools.el -*- lexical-binding: t; -*-

(straight-use-package 'citre)
(straight-use-package 'eglot)
(straight-use-package 'consult-eglot)
(straight-use-package 'edit-indirect)

(use-package citre
    :init
    (require 'citre-config)
    (setq citre-tags-completion-case-sensitive nil)

    (add-hook 'emacs-lisp-mode-hook #'my/do-not-use-citre-imenu)
    (add-hook 'emacs-lisp-mode-hook #'my/do-not-use-citre-capf)
    (add-hook 'emacs-lisp-mode-hook #'my/do-not-use-citre-xref)
    (add-hook 'org-mode-hook #'my/do-not-use-citre-imenu)
    (add-hook 'org-mode-hook #'my/do-not-use-citre-capf)
    (add-hook 'markdown-mode-hook #'my/do-not-use-citre-imenu)

    :config

    (general-define-key
     :keymaps 'citre-mode-map
     :states '(normal motion)
     "C-]" #'citre-jump
     "C-t" #'citre-jump-back)

    (general-define-key
     :keymaps 'citre-mode-map
     :states '(normal motion visual)
     "C-w ]" #'citre-peek)

    (general-define-key
     :prefix "SPC w"
     :non-normal-prefix "M-SPC w"
     :keymaps 'citre-mode-map
     :states '(normal visual insert motion)
     "]" #'citre-peek)

    (general-define-key
     :keymaps 'citre-peek-keymap
     :states '(normal motion)
     "q" #'citre-peek-abort
     "RET" #'citre-peek-jump
     "[t" #'citre-peek-prev-definition
     "]t" #'citre-peek-next-definition
     "M-[" #'citre-peek-prev-line
     "M-]" #'citre-peek-next-line
     "M-{" #'citre-peek-prev-branch
     "M-}" #'citre-peek-next-branch)

    (add-hook 'citre-mode-hook #'evil-normalize-keymaps)
    (add-hook 'citre-peek--mode-hook #'evil-normalize-keymaps)
    (add-hook 'citre-after-jump-hook #'better-jumper-set-jump)

    )

(use-package eldoc
    :init
    (setq eldoc-echo-area-use-multiline-p nil)

    :config
    (add-to-list 'display-buffer-alist
                 `("\\*eldoc\\*"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.5)
                   (window-height . 0.4)
                   (slot . ,(alist-get 'eldoc my/side-window-slots))))

    )

(use-package eglot
    :init
    (setq eglot-stay-out-of '("company")
          eglot-workspace-configuration
          '(:pyright (:useLibraryCodeForTypes t :openFilesOnly :json-false)
            :r (:lsp (:diagnostics :json-false)))
          read-process-output-max (* 1024 1024))

    :config
    (add-to-list 'eglot-server-programs
                 '(python-mode . ("pyright-langserver" "--stdio")))

    (add-hook
     'eglot-managed-mode-hook #'my/toggle-citre-eglot-capf)

    ;; NOTE: THIS IS REALLY IMPORTANT!
    ;; when you are registered evil keymaps for a minor mode keymap
    ;; you MUST call this func to automatically activate them
    ;; otherwise, you have to make a state transistion to make them
    ;; become effective.
    (add-hook 'eglot-managed-mode-hook #'evil-normalize-keymaps)

    (general-create-definer my/lsp-map
        :prefix "SPC l"
        :non-normal-prefix "M-SPC l"
        :prefix-map 'my/lsp-map)

    (my/lsp-map
        :keymaps 'eglot-mode-map
        :states '(normal insert motion visual)
        "" '(:ignore t :which-key "lsp")
        "f" #'eglot-format
        "s" #'consult-eglot-symbols
        "a" #'eglot-code-actions
        "e" #'consult-flymake
        "n" #'eglot-rename
        "[" #'xref-go-back
        "]" #'xref-go-forward)

    (general-define-key
     :keymaps 'eglot-mode-map
     :states '(normal motion)
     "] d" #'flymake-goto-next-error
     "[ d" #'flymake-goto-prev-error
     ;; jump to next/prev location containing the references.
     "[ r" (my/xref-move-in-original-src-macro xref-prev-line)
     "] r" (my/xref-move-in-original-src-macro xref-next-line)
     ;; jump to next/prev file containing the references.
     "[ R" (my/xref-move-in-original-src-macro xref-prev-group)
     "] R" (my/xref-move-in-original-src-macro xref-next-group)
     "K" #'my/eldoc-buffer-dwim
     "gd" #'xref-find-definitions
     "gr" #'xref-find-references)

    )

(use-package edit-indiret
    :init
    (add-hook 'edit-indirect-after-creation-hook #'my/markdown-src-lsp-setup)
    (add-to-list 'display-buffer-alist
                 '("\\*edit-indirect"
                   (display-buffer-at-bottom)
                   (window-height . 0.8))))

(provide 'my-init-langtools)
;;; my-init-langtools.el ends here
