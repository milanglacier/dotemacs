;;; my-init-elisp.el -*- lexical-binding: t; -*-

(straight-use-package 'helpful)
(straight-use-package 'elisp-demos)
(straight-use-package 'highlight-quoted)
(straight-use-package 'macrostep)
(straight-use-package '(elispfl :host github :repo "cireu/elispfl"))
;; fontification for elisp

(use-package helpful
    :init
    (general-define-key
     [remap describe-function] #'helpful-callable
     [remap describe-command] #'helpful-command
     [remap describe-variable] #'helpful-variable
     [remap describe-key] #'helpful-key
     [remap describe-symbol] #'helpful-symbol)

    (setq helpful-switch-buffer-function #'my/helpful-display-buffer)

    :config
    (general-define-key
     :keymaps 'helpful-mode-map
     :states '(motion normal)
     "K" #'my/helpful-lookup-symbl-at-point))

(use-package elisp-demos
    :init
    (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package elisp-mode
    :init
    (add-hook 'emacs-lisp-mode-hook #'my/elisp-setup)

    :config
    (my/define-and-bind-local-paren-text-object "`" "`" "'" emacs-lisp-mode-hook)
    (elispfl-mode)
    (setq lisp-body-indent 4
          lisp-indent-function #'my/lisp-indent-function)

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


(provide 'my-init-elisp)
;; my-init-elisp.el ends here
