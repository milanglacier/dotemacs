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
                   (side . bottom)
                   (slot . ,(alist-get 'R my/side-window-slots))))

    (add-to-list 'display-buffer-alist
                 `("^\\*R Dired"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.5)
                   (window-height . 0.5)
                   (side . right)
                   (slot . ,(alist-get 'Rdired my/side-window-slots))))

    (add-to-list 'display-buffer-alist
                 `("^\\*help\\[R\\]"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.5)
                   (window-height . 0.5)
                   (side . bottom)
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

(use-package python
    :init
    (defvar my/python-enable-ipython t
        "use ipython as the embedded REPL.")
    (setq python-indent-offset 4)

    :config
    (add-to-list 'python-mode-hook #'eglot-ensure)

    (evil-define-operator my/send-region-to-python (beg end)
        (python-shell-send-region beg end t))

    (my/localleader
        :keymaps 'python-mode-map
        :states '(normal visual insert motion)
        "s" #'my/send-region-to-python)

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
    (add-hook 'polymode-switch-buffer-hook
              (defun my/poly-mode-disable-flymake (old-buf new-buf)
                  "poly-mode are duplicated buffers with exactly the
same buffer content, when you are on `prog-mode' then the your code
linter will be perplexed by those prose content. So disable flymake in
poly-mode."
                  (with-current-buffer new-buf
                      (when flymake-mode
                          (flymake-mode -1))))))

(use-package quarto-mode
    :commands poly-quarto-mode)

(use-package xref
    :config
    (defmacro my/xref-move-in-original-src-macro (func)
        "There can only be one xref buffer. That is, if you find
references of other symbol the previous one will be overwritten. The
official `xref-next-line' `xref-next-group' only allows you to move
the location in the src buffer when your point is in the xref buffer
window. This macro creates funcs that allow you to move current window
to next xref location."
        (let ((xref-move-func (intern (format "my/%s" func)))
              (xref-move-func-desc (format "Effectively calling %s in the src window." func)))
            `(defun ,xref-move-func ()
                 ,xref-move-func-desc
                 (interactive)
                 (with-current-buffer "*xref*"
                     (funcall ',func)))))

    )

(use-package eldoc
    :init
    (setq eldoc-echo-area-use-multiline-p nil)
    (defvar-local my/eldoc-buffer-dwim-key "K"
        "The key to enable dwim behavior on displaying eldoc buffer")

    (defun my/eldoc-buffer-dwim ()
        "When eldoc buffer window is not opened, display the eldoc
window. Pressing `my/eldoc-buffer-dwim-key' again within a short
period (1s currently as hard coded) will move your focus on the eldoc
window. If the shorter period has gone, calling this command will
close the eldoc window. Currently this dwim hack is only effective in
`eglot-mode-map' as it is hardcoded."
        (interactive)
        (if-let ((eldoc-win (get-buffer-window "*eldoc*")))
                (delete-window eldoc-win)
            (progn
                (eldoc-doc-buffer)
                (my/eldoc-dwim-hack))))

    :config
    (add-to-list 'display-buffer-alist
                 `("\\*eldoc\\*"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.5)
                   (window-height . 0.5)
                   (slot . ,(alist-get 'eldoc my/side-window-slots))))

    (defun my/eldoc-dwim-hack ()
        "bind `my/eldoc-buffer-dwim-key' locally to a command that
will switch to the eldoc buffer, and unbind the key after a short
period (1s as hard coded.)"
        (general-define-key
         :keymaps 'eglot-mode-map
         :states '(normal motion)
         my/eldoc-buffer-dwim-key #'my/eldoc-focus)

        (run-with-idle-timer 1 nil #'my/eldoc-locally-unbind))

    (defun my/eldoc-locally-unbind ()
        (general-define-key
         :states '(normal motion)
         :keymaps 'eglot-mode-map
         my/eldoc-buffer-dwim-key #'my/eldoc-buffer-dwim))

    (defun my/eldoc-focus ()
        "focus on the eldoc window"
        (interactive)
        (when (get-buffer-window "*eldoc*")
            (select-window (get-buffer-window "*eldoc*"))))

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

(provide 'my-init-langs)
;;; my-init-langs.el ends here
