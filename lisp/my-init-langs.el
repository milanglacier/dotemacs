;;; my-init-langs.el -*- lexical-binding: t; -*-

;; ess
(straight-use-package 'ess)
(straight-use-package 'poly-R)
(straight-use-package 'quarto-mode)

;; json
(straight-use-package 'json-mode)

(use-package ess
    :init
    (setq comint-scroll-to-bottom-on-input t
          comint-scroll-to-bottom-on-output t
          ess-imenu-use-S nil ;; imenu integration in rmarkdown causes emacs hang
          ess-imenu-use-p nil
          ess-indent-offset 4
          ess-use-flymake nil)

    :config
    (add-to-list 'display-buffer-alist
                 `("^\\*R:"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.5)
                   (window-height . 0.5)
                   (slot . ,(alist-get 'R my/side-window-slots))))

    (add-to-list 'display-buffer-alist
                 `("^\\*R Dired"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.5)
                   (window-height . 0.5)
                   (side . ,(alist-get 'Rdired my/side-window-sides))
                   (slot . ,(alist-get 'Rdired my/side-window-slots))))

    (add-to-list 'display-buffer-alist
                 `("^\\*help\\[R\\]"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.5)
                   (window-height . 0.5)
                   (slot . ,(alist-get 'Rhelp my/side-window-slots))))

    (evil-set-initial-state 'ess-r-help-mode 'normal)

    (my/define-and-bind-local-paren-text-object " c" "# %%" "# %%" ess-r-mode-hook)
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
        "v" '(:ignore t :which-key "view")
        "vh" #'my/ess-toggle-view-httpgd)

    (add-hook 'ess-r-mode-hook #'my/ess-set-company-backend)
    (add-hook 'ess-r-mode-hook #'my/eglot-do-not-use-imenu)
    (add-hook 'ess-r-mode-hook #'eglot-ensure)
    (add-hook 'ess-r-mode-hook #'my/ess-set-tab-width-4)

    )

(use-package python
    :init
    (defvar my/python-enable-ipython t
        "use ipython as the embedded REPL.")
    (setq python-indent-offset 4)

    :config
    (add-to-list 'python-mode-hook #'eglot-ensure)

    (my/localleader
        :keymaps 'python-mode-map
        :states '(normal visual insert motion)
        "s" #'my/send-region-to-python
        "v" '(:ignore t :which-key "view")
        "vh" #'my/python-toggle-view-local-html)

    (when my/python-enable-ipython
        (setq python-shell-interpreter "ipython3")
        (setq python-shell-interpreter-args "--i --simple-prompt --no-color-info"))

    (add-to-list 'display-buffer-alist
                 `("\\*[pP]ython\\*"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.5)
                   (window-height . 0.5)
                   (side . bottom)
                   (slot . ,(alist-get 'python my/side-window-slots))))
    )

(use-package polymode-core
    :config
    (add-hook 'polymode-switch-buffer-hook #'my/poly-mode-disable-flymake))

(use-package quarto-mode
    :commands poly-quarto-mode)

(provide 'my-init-langs)
;;; my-init-langs.el ends here
