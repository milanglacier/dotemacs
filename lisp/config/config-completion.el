;;; config-completion.el -*- lexical-binding: t; -*-

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
          company-global-modes '(not vterm-mode eat-mode)
          company-frontends '(company-pseudo-tooltip-frontend
                              ;; always show candidates in overlay tooltip
                              company-echo-metadata-frontend)
          company-backends '((company-files company-yasnippet company-capf :separate company-dabbrev-code))
          company-auto-commit nil
          company-dabbrev-other-buffers nil
          company-dabbrev-code-other-buffers nil
          company-dabbrev-ignore-case t
          company-dabbrev-code-ignore-case t
          ;; provide dabbrev completion inside comments and strings
          company-dabbrev-code-everywhere t
          company-dabbrev-downcase nil
          company-selection-wrap-around t
          completion-ignore-case t)

    (mg-run-hook-once evil-insert-state-entry-hook global-company-mode)
    (mg-run-hook-once evil-insert-state-entry-hook company-tng-mode)
    (mg-setq-on-hook text-mode-hook
                     company-backends
                     '((company-files company-yasnippet company-capf :separate company-dabbrev)))

    :config

    (with-eval-after-load 'company-files
        ;; Fix `company-files' completion for org file:* links
        (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)"))

    (unless (display-graphic-p)
        ;; Don't persist company popups when switching back to normal mode.
        ;; `company-box' aborts on mode switch so it doesn't need this.
        (add-hook 'evil-normal-state-entry-hook #'mg-company-abort))

    (when (display-graphic-p)
        (add-hook 'company-mode-hook #'company-box-mode))

    (add-hook 'company-mode-hook #'evil-normalize-keymaps)

    (general-define-key
     :keymaps 'company-active-map
     "C-e" #'company-abort
     ;; use C-y to enter yasnippet expansion
     ;; without input of additional character.
     "C-y" #'company-complete-selection)

    (general-define-key
     :keymaps 'company-mode-map
     :states 'insert
     ;; manually invoke the completion
     "M-i" #'company-manual-begin
     "M-c M-c" #'mg-complete-ctags
     "M-c M-f" #'cape-file
     "M-c M-d" #'cape-dabbrev)

    (advice-add #'company-capf :around #'mg-company-completion-styles)

    (yas-global-mode))

(use-package company-box
    :config
    (setq company-box-max-candidates 50
          company-frontends '(company-tng-frontend company-box-frontend)))

(use-package yasnippet
    :init
    (setq yas-verbosity 2))

(provide 'config-completion)
;;; config-completion.el ends here
