;;; my-init-completion.el -*- lexical-binding: t; -*-

(straight-use-package 'company)
(straight-use-package 'company-box)
(straight-use-package 'cape)

(straight-use-package 'yasnippet)

(use-package company

    :init
    (setq company-minimum-prefix-length 2
          company-dabbrev-minimum-length 3
          company-dabbrev-code-time-limit 0.3
          company-tooltip-limit 14
          company-tooltip-align-annotations t
          company-require-match 'never
          company-files-exclusions '(".git/" ".DS_Store")
          company-global-modes '(not vterm-mode)
          company-frontends '(company-pseudo-tooltip-frontend
                              ;; always show candidates in overlay tooltip
                              company-echo-metadata-frontend)
          company-backends '((company-files company-yasnippet company-capf :separate company-dabbrev-code))
          company-auto-commit nil
          company-dabbrev-other-buffers nil
          company-dabbrev-code-other-buffers nil
          company-dabbrev-ignore-case t
          company-dabbrev-code-ignore-case t
          company-dabbrev-downcase nil
          company-selection-wrap-around t
          completion-ignore-case t)

    (my/run-hook-once evil-insert-state-entry-hook global-company-mode)
    (my/run-hook-once evil-insert-state-entry-hook company-tng-mode)
    (my/setq-on-hook text-mode-hook
                     company-backends
                     '((company-files company-yasnippet company-capf :separate company-dabbrev)))

    :config
    (add-hook 'company-mode-hook #'evil-normalize-keymaps)
    (evil-make-overriding-map company-mode-map)
    (evil-make-overriding-map company-active-map)

    (setq company-text-icons-mapping
          '((array "" font-lock-type-face)
            (boolean "" font-lock-builtin-face)
            (class "" font-lock-type-face)
            (color "" success)
            (constant "" font-lock-constant-face)
            (constructor "" font-lock-function-name-face)
            (enum-member "" font-lock-builtin-face)
            (enum "" font-lock-builtin-face)
            (field "ﰠ" font-lock-variable-name-face)
            (file "" font-lock-string-face)
            (folder "" font-lock-doc-face)
            (interface "" font-lock-type-face)
            (keyword "" font-lock-keyword-face)
            (method "" font-lock-function-name-face)
            (function "" font-lock-function-name-face)
            (module "" font-lock-type-face)
            (numeric "" font-lock-builtin-face)
            (operator "" font-lock-comment-delimiter-face)
            (property "ﰠ" font-lock-variable-name-face)
            (reference "" font-lock-doc-face)
            (snippet "" font-lock-string-face)
            (string "" font-lock-string-face)
            (struct "פּ" font-lock-variable-name-face)
            (text "" shadow)
            (type-parameter "" font-lock-type-face)
            (unit "" shadow)
            (value "" font-lock-builtin-face)
            (variable "" font-lock-variable-name-face)
            (t "" shadow)))

    (unless (display-graphic-p)
        ;; Don't persist company popups when switching back to normal mode.
        ;; `company-box' aborts on mode switch so it doesn't need this.
        (add-hook 'evil-normal-state-entry-hook #'my/company-abort))

    (with-eval-after-load 'company-files
        ;; Fix `company-files' completion for org file:* links
        (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)"))

    (when (display-graphic-p)
        (add-hook 'company-mode-hook #'company-box-mode))

    (general-define-key
     :keymaps 'company-active-map
     "C-e" #'company-abort
     ;; use C-y to enter yasnippet expansion
     ;; without input of additional character.
     "C-y" #'company-complete-selection)

    (general-define-key
     :keymaps
     'company-mode-map
     ;; manually invoke the completion
     "M-i" #'company-complete)

    (advice-add #'company-capf :around #'my/company-completion-styles)

    (yas-global-mode))

(use-package company-box
    :config
    (setq company-box-max-candidates 50
          company-frontends '(company-tng-frontend company-box-frontend)
          my$company-box-icons-alist
          '((Unknown . "")
            (Text . "")
            (Method . "")
            (Function . "")
            (Constructor . "")
            (Field . "")
            (Variable . "")
            (Class . "")
            (Interface . "")
            (Module . "")
            (Property . "")
            (Unit . "")
            (Value . "")
            (Enum . "")
            (Keyword . "")
            (Snippet . "")
            (Color . "")
            (File . "")
            (Reference . "")
            (Folder . "")
            (EnumMember . "")
            (Constant . "")
            (Struct . "פּ")
            (Event . "")
            (Operator . "")
            (TypeParameter . "")
            (Template . ""))
          company-box-icons-alist 'my$company-box-icons-alist))

(use-package yasnippet
    :init
    (setq yas-verbosity 2))

(provide 'my-init-completion)
;;; my-init-completion.el ends here
