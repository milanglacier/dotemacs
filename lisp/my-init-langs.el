;;; my-init-langs.el -*- lexical-binding: t; -*-

;; ess
(straight-use-package 'ess)

;; markdown
(straight-use-package 'markdown-mode)

;; sql
(straight-use-package 'sql-indent)

(use-package ess
    :init
    (setq comint-scroll-to-bottom-on-input t
          comint-scroll-to-bottom-on-output t
          ess-imenu-use-S nil ;; imenu integration in rmarkdown causes emacs hang
          ess-imenu-use-p nil
          ess-indent-offset 4
          ess-use-flymake nil
          ;; NOTE: when a comment starts with `# %%', it will be
          ;; treated as a code cell which can be converted to the
          ;; jupyter notebook format.
          ;; `ess-indent-with-fancy-comments' is set to nil to avoid
          ;; the "peculiar" indentation behavior of ess for such
          ;; "fancy" comments.
          ess-indent-with-fancy-comments nil)

    :config
    (add-to-list 'display-buffer-alist
                 `("^\\*R:"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.5)
                   (window-height . 0.4)
                   (slot . ,(alist-get 'R my/side-window-slots))))

    (add-to-list 'display-buffer-alist
                 `("^\\*R Dired"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.33)
                   (window-height . 0.4)
                   (side . ,(alist-get 'Rdired my/side-window-sides))
                   (slot . ,(alist-get 'Rdired my/side-window-slots))))

    (add-to-list 'display-buffer-alist
                 `("^\\*R Watch"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.33)
                   (window-height . 0.4)
                   (side . ,(alist-get 'RWatch my/side-window-sides))
                   (slot . ,(alist-get 'RWatch my/side-window-slots))))

    (add-to-list 'display-buffer-alist
                 `("^\\*help\\[R\\]"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.5)
                   (window-height . 0.4)
                   (slot . ,(alist-get 'Rhelp my/side-window-slots))))

    (evil-set-initial-state 'ess-r-help-mode 'normal)

    (my/define-and-bind-local-paren-text-object " c" "# %%" "# %%" ess-r-mode-hook)
    (my/define-and-bind-local-paren-text-object "m" "# COMMAND ----------" "# COMMAND ----------" ess-r-mode-hook)

    (setq ess-R-font-lock-keywords
          '((ess-R-fl-keyword:keywords . t)
            (ess-R-fl-keyword:constants . t)
            (ess-R-fl-keyword:modifiers . t)
            (ess-R-fl-keyword:fun-defs . t)
            (ess-R-fl-keyword:assign-ops . t)
            (ess-R-fl-keyword:%op% . t)
            (ess-fl-keyword:fun-calls . t)
            (ess-fl-keyword:numbers . t)
            (ess-fl-keyword:operators . t)
            (ess-fl-keyword:delimiters . t)
            (ess-fl-keyword:= . t)
            (ess-R-fl-keyword:F&T . t)))

    (my/localleader
        :keymaps 'ess-mode-map
        :states '(normal visual motion insert)
        "s" #'my/send-region-to-ess
        "r" '(:ignore t :which-key "repl")
        "rs" #'run-ess-r
        "v" '(:ignore t :which-key "view")
        "vh" #'my/ess-toggle-view-httpgd)

    (my/setq-on-hook ess-r-mode-hook company-backends
                     '((company-files company-yasnippet company-capf
                                      company-R-args company-R-objects
                                      :separate company-dabbrev-code)))

    (add-hook 'ess-r-mode-hook (my/setq-locally eglot-stay-out-of '(company imenu)))
    (add-hook 'ess-r-mode-hook #'eglot-ensure)
    (add-hook 'ess-r-mode-hook (my/setq-locally tab-width 4))
    (when (and (display-graphic-p)
               (featurep 'xwidget-internal))
        (add-hook 'ess-r-mode-hook #'my/xwidget-side-window-mode))

    )

(use-package python
    :init
    (setq python-indent-offset 4)

    :config
    (add-hook 'python-ts-mode-hook #'eglot-ensure)
    (add-hook 'python-ts-mode-hook (my/setq-locally tab-width 4))
    (add-hook 'python-ts-mode-hook #'my:treesit-embed-sql-in-python-setup)

    (my/define-and-bind-local-paren-text-object " c" "# %%" "# %%" python-ts-mode-hook)
    (my/define-and-bind-local-paren-text-object "m" "# COMMAND ----------" "# COMMAND ----------" python-ts-mode-hook)

    (my/localleader
        :keymaps 'python-ts-mode-map
        :states '(normal visual insert motion)
        "s" #'my~ipython-send-region-operator
        "r" '(:ignore t :which-key "REPL")
        "rs" #'my~ipython-start
        "v" '(:ignore t :which-key "view")
        "vh" #'my/python-toggle-view-local-html)

    (add-to-list 'display-buffer-alist
                 `("^\\*ipython"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.5)
                   (window-height . 0.4)
                   (side . bottom)
                   (slot . ,(alist-get 'python my/side-window-slots))))

    (when (and (display-graphic-p)
               (featurep 'xwidget-internal))
        (add-hook 'python-ts-mode-hook #'my/xwidget-side-window-mode)
        (add-hook 'python-ts-mode-hook #'my/refresh-xwidget-after-eval-python-mode))

    )

(use-package markdown-mode
    :mode (("\\.[Rr]md\\'" . markdown-mode)
           ("\\.qmd\\'" . markdown-mode))
    :init
    (setq markdown-fontify-code-blocks-natively t
          markdown-fontify-whole-heading-line t
          markdown-enable-math t)

    :config

    (general-define-key
     :states '(normal motion)
     :keymaps 'markdown-mode-map
     "TAB" #'markdown-cycle)

    (my/define-and-bind-local-paren-text-object "c" "```.+$" "^```$" markdown-mode-hook)

    (my/localleader
        :states '(normal insert visual motion)
        :keymaps 'markdown-mode-map
        "r" '(:ignore t :which-key "repl")
        "rs" #'my/markdown-run-repl
        "s" #'my/markdown-send-region)

    (add-hook 'markdown-mode-hook #'eglot-ensure)

    (add-to-list 'markdown-code-lang-modes '("R" . ess-r-mode))
    (add-to-list 'markdown-code-lang-modes '("r" . ess-r-mode))

    )

(use-package go-ts-mode
    :config
    (add-hook 'go-ts-mode-hook (my/setq-locally tab-width 4))
    (add-hook 'go-ts-mode-hook #'eglot-ensure))

(use-package rust-ts-mode
    :config
    (add-hook 'rust-ts-mode-hook #'prettify-symbols-mode)
    (add-hook 'rust-ts-mode-hook #'eglot-ensure))

(use-package sql
    :init
    (setq sqlind-basic-offset 4)

    :config
    (add-hook 'sql-mode-hook (my/setq-locally tab-width 4))
    (my/define-and-bind-local-paren-text-object "m" "-- COMMAND ----------" "-- COMMAND ----------" sql-mode-hook))

(provide 'my-init-langs)
;;; my-init-langs.el ends here
