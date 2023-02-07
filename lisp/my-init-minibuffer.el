;;; my-init-minibuffer.el -*- lexical-binding: t; -*-

(straight-use-package '(vertico :host github :repo "minad/vertico"
                                :files (:defaults "extensions/*.el")))
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'compat)
(straight-use-package 'consult-dir)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'marginalia)
(straight-use-package 'wgrep)
(straight-use-package 'all-the-icons-completion)

(use-package vertico
    :init
    (my/run-hook-once pre-command-hook vertico-mode)

    (defun crm-indicator (args)
        (cons (format "[CRM%s] %s"
                      (replace-regexp-in-string
                       "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                       crm-separator)
                      (car args))
              (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    (general-create-definer my/find-map
        :prefix "SPC f"
        :non-normal-prefix "M-SPC f"
        :prefix-map 'my/find-map)

    (my/find-map
        :keymaps 'override
        :states '(motion normal)
        "" '(:ignore t :which-key "find")
        "f" #'project-find-file
        "F" #'find-file
        "d" #'project-dired
        "o" #'consult-recent-file
        "r" #'consult-yank-from-kill-ring
        "b" #'consult-buffer
        "p" #'project-switch-project
        "g" #'consult-ripgrep
        "a" #'embark-act
        "j" #'evil-collection-consult-jump-list
        "m" #'evil-collection-consult-mark
        "i" #'consult-imenu
        "I" #'consult-imenu-multi)

    :config
    (setq vertico-resize nil
          vertico-count 17
          vertico-cycle t)
    (setq-default completion-in-region-function
                  (lambda (&rest args)
                      (apply (if vertico-mode
                                     #'consult-completion-in-region
                                 #'completion--in-region)
                             args)))

    ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
    ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

    (general-define-key
     :keymaps 'vertico-map
     "DEL" #'vertico-directory-delete-char
     "C-k" #'kill-line
     "C-p" #'previous-line-or-history-element
     "C-n" #'next-line-or-history-element
     "C-u" #'evil-delete-back-to-indentation)

    (general-define-key
     :keymaps '(minibuffer-local-map read-expression-map)
     "C-k" #'kill-line
     "C-p" #'previous-line-or-history-element
     "C-n" #'next-line-or-history-element
     "ESC" #'abort-recursive-edit
     "C-u" #'evil-delete-back-to-indentation)

    )

(use-package orderless
    :demand t
    :config
    (setq completion-styles '(orderless basic)
          completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
    :init
    (my/run-hook-once pre-command-hook marginalia-mode)
    :config
    (if (display-graphic-p)
            (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)))

(use-package consult
    :init
    (general-define-key
     [remap apropos] #'consult-apropos
     [remap bookmark-jump] #'consult-bookmark
     [remap evil-show-marks] #'consult-mark
     [remap evil-show-jumps] #'evil-collection-consult-jump-list
     [remap evil-show-registers] #'consult-register
     [remap goto-line] #'consult-goto-line
     [remap imenu] #'consult-imenu
     [remap locate] #'consult-locate
     [remap load-theme] #'consult-theme
     [remap man] #'consult-man
     [remap recentf-open-files] #'consult-recent-file
     [remap switch-to-buffer] #'consult-buffer
     [remap switch-to-buffer-other-window] #'consult-buffer-other-window
     [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame
     [remap yank-pop] #'consult-yank-pop)

    :config
    (setq consult-narrow-key "<"
          consult-line-numbers-widen t
          consult-async-min-input 2
          consult-async-refresh-delay 0.15
          consult-async-input-throttle 0.2
          consult-async-input-debounce 0.1)
    )

(use-package consult-dir
    :init
    (general-define-key
     [remap list-directory] #'consult-dir)
    (general-define-key
     :keymaps 'vertico-map
     "C-c C-d " #'consult-dir
     "C-c C-j" #'consult-dir-jump-file)

    :config
    ;; TODO: doomemacs configures docker paths for consult dir
    ;; when docker-tramp is configured, will take a reference from it.
    (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t)
    (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t))

(use-package embark
    :init
    (general-define-key
     [remap describe-bindings] #'embark-bindings)

    (setq which-key-use-C-h-commands nil
          prefix-help-command #'embark-prefix-help-command)

    (general-define-key
     :kemaps 'minibuffer-local-map
     "C-;" #'embark-act
     "C-c C-a" #'embark-act
     "C-c C-e" #'embark-export
     "C-c C-l" #'embark-collect)

    (my/leader
        :keymaps 'override
        :states '(visual insert motion)
        "a" #'embark-act)

    :config
    (require 'consult)
    ;; useful when use embark-export that exports to a grep-mode buffer
    (evil-set-initial-state 'grep-mode 'normal))

(use-package wgrep
    :commands (wgrep-change-to-wgrep-mode wgrep-setup)
    :config
    (setq wgrep-auto-save-buffer t)

    ;; copied from doomemacs
    (evil-define-operator my/evil-delete-in-wgrep (beg end type register yank-handler)
        "A wrapper around `evil-delete' for `wgrep' buffers that will invoke
`wgrep-mark-deletion' on lines you try to delete."
        (interactive "<R><x><y>")
        (condition-case _ex
                (evil-delete beg end type register yank-handler)
            ('text-read-only
             (evil-apply-on-block
              (lambda (beg _)
                  (goto-char beg)
                  (call-interactively #'wgrep-mark-deletion))
              beg (1- end) nil))))

    (general-define-key
     :keymaps 'wgrep-mode-map
     [remap evil-delete] #'my/evil-delete-in-wgrep))

(provide 'my-init-minibuffer)
;;; my-init-minibuffer.el ends here
