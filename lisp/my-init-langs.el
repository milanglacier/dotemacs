;;; my-init-langs.el -*- lexical-binding: t; -*-
(straight-use-package 'citre)
(straight-use-package 'eglot)
(straight-use-package 'consult-eglot)

;; ess
(straight-use-package 'ess)
(straight-use-package 'poly-R)
(straight-use-package 'quarto-mode)

;; json
(straight-use-package 'json-mode)

(use-package citre
    :defer t
    :init
    (require 'citre-config)
    (setq citre-tags-completion-case-sensitive nil)

    (defun my/do-not-use-citre-imenu ()
        (setq-local citre-enable-imenu-integration nil))
    (defun my/do-not-use-citre-xref ()
        (setq-local citre-enable-xref-integration nil))
    (defun my/do-not-use-citre-capf ()
        (setq-local citre-enable-capf-integration nil))

    (add-hook 'emacs-lisp-mode-hook #'my/do-not-use-citre-imenu)
    (add-hook 'emacs-lisp-mode-hook #'my/do-not-use-citre-capf)
    (add-hook 'emacs-lisp-mode-hook #'my/do-not-use-citre-xref)
    (add-hook 'org-mode-hook #'my/do-not-use-citre-imenu)
    (add-hook 'org-mode-hook #'my/do-not-use-citre-capf)

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

    (my/window-map
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

(use-package ess
    :defer t
    :init
    (setq comint-scroll-to-bottom-on-input t
          comint-scroll-to-bottom-on-output t
          ess-imenu-use-S nil ;; imenu integration in rmarkdown causes emacs hang
          ess-imenu-use-p nil
          ess-indent-offset 4
          ess-use-flymake nil)

    :config
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

    (evil-define-operator my/send-region-to-ess (beg end)
        "This operator sends the region (either motion or text objects) to ess REPL"
        ;; t means don't echo the region in the ess REPL buffer
        (ess-eval-region beg end t))

    (my/localleader
        :keymaps 'ess-mode-map
        :states '(normal visual motion insert)
        "s" #'my/send-region-to-ess)

    (defun my/ess-set-company-backend ()
        (setq-local company-backends
                    '((company-capf company-files
                                    company-R-library company-R-args company-R-objects
                                    :separate company-dabbrev
                                    :with company-yasnippet))))

    (add-hook 'ess-r-mode-hook #'my/ess-set-company-backend)
    (add-hook 'ess-r-mode-hook #'my/eglot-do-not-use-imenu)
    (add-hook 'ess-r-mode-hook #'eglot-ensure)
    (add-hook 'ess-r-mode-hook (defun my/set-tab-width-4 ()
                                   (setq-local tab-width 4)))

    )

(use-package eglot
    :defer t
    :init
    (setq eglot-stay-out-of '("company"))

    (defun my/eglot-do-not-use-imenu ()
        (add-to-list 'eglot-stay-out-of "imenu"))

    :config
    (add-to-list 'eglot-server-programs
                 '(python-mode . ("pyright-langserver" "--stdio")))

    (defalias #'my/eglot-citre-capf
        (cape-super-capf #'eglot-completion-at-point #'citre-completion-at-point))

    (add-hook
     'eglot-managed-mode-hook
     (defun my/toggle-citre-eglot-capf ()
         (if (eglot-managed-p)
                 (add-to-list 'completion-at-point-functions #'my/eglot-citre-capf)
             (setq-local completion-at-point-functions
                         (delq #'my/eglot-citre-capf completion-at-point-functions)))))

    )

(provide 'my-init-langs)
;;; my-init-langs.el ends here
