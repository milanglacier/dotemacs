;;; my-iniy-elisp.el -*- lexical-binding: t; -*-

(straight-use-package 'helpful)
(straight-use-package 'elisp-demos)
(straight-use-package 'highlight-quoted)
(straight-use-package 'macrostep)
(straight-use-package '(elispfl :host github :repo "cireu/elispfl"))
;; fontification for elisp

(use-package helpful
    :defer t
    :init
    (general-define-key
     [remap describe-function] #'helpful-callable
     [remap describe-command] #'helpful-command
     [remap describe-variable] #'helpful-variable
     [remap describe-key] #'helpful-key
     [remap describe-symbol] #'helpful-symbol)
    :config
    (general-define-key
     :keymaps 'helpful-mode-map
     :states '(motion normal)
     "K" (defun my/helpful-lookup-symbl-at-point ()
             (interactive)
             (helpful-symbol (symbol-at-point)))))

(use-package elisp-demos
    :defer t
    :init
    (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

;; TODO: I don't know why I can't use `use-package' with 'elisp-mode
(with-eval-after-load 'elisp-mode

    (elispfl-mode)

    (setq lisp-body-indent 4)

    (defun my/elisp-loop-up-symbol (beg end)
        "Look up for the symbol under point, if region is active, use the selected region as the symbol"
        (interactive "r")
        (if (use-region-p)
                (helpful-symbol (intern (buffer-substring beg end)))
            (helpful-symbol (symbol-at-point))))

    (general-define-key
     :states '(motion visual normal)
     :keymaps 'emacs-lisp-mode-map
     "K" #'my/elisp-loop-up-symbol)

    (my/localleader
        :keymaps 'emacs-lisp-mode-map
        :states '(motion visual insert normal)
        "m" #'macrostep-expand
        "e" '(:ignore t :which-key "eval")
        "ee" #'eval-last-sexp
        "ef" #'eval-defun
        "eb" #'eval-buffer
        "er" #'eval-region))

(defun my/elisp-setup ()
    ;; referenced from doomemacs

    (my/define-and-bind-local-paren-text-object "`" "`" "'" emacs-lisp-mode-hook)

    (defun my/emacs-lisp-outline-level ()
        "Return outline level for comment at point.
Intended to replace `lisp-outline-level'."
        (- (match-end 1) (match-beginning 1)))

    (setq-local outline-regexp "[ \t]*;;;\\(;*\\**\\) [^ \t\n]"
                outline-level #'my/emacs-lisp-outline-level)
    (outline-minor-mode)
    (highlight-quoted-mode))

(add-hook 'emacs-lisp-mode-hook #'my/elisp-setup)

(provide 'my-init-elisp)
;; my-init-elisp.el ends here
