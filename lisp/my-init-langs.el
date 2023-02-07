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

    ;; TODO: look up ESS manual section #3.5
    ;; for configuring displaying R related buffer.
    )

(use-package xref
    :init

    (defmacro my/xref-move-in-original-src-macro (func)
        "There can only be one xref buffer. That is, if you find
references of other symbol the previous one will be overwritten. The
official `xref-next-line' `xref-next-group' only allows you to move
the location in the src buffer when your point is in the xref buffer
window. This macro creates funcs that allow you to move current window
to next xref location."
        (let ((xref-move-func (intern (format "my/%s" func)))
              (xref-move-func-desc (format "Effectively calling %s in the src window." func)))
            `(cl-defun ,xref-move-func ()
                 ,xref-move-func-desc
                 (interactive)
                 (dolist (buf (buffer-list))
                     (with-current-buffer buf
                         (when (equal "*xref*" (buffer-name))
                             (funcall ',func)
                             (cl-return-from ,xref-move-func)))))))

    )

(use-package eglot
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
        "n" #'eglot-rename)

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
     "K" #'eldoc-doc-buffer
     "gd" #'xref-find-definitions
     "gr" #'xref-find-references)
    )

(use-package consult-eglot
    :init
    (my/find-map
        :keymaps 'eglot-mode-map
        :states '(normal visual insert motion)
        "l" #'consult-eglot-symbols))

(provide 'my-init-langs)
;;; my-init-langs.el ends here
