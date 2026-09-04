;;; config-completion-corfu.el -*- lexical-binding: t; -*-

(straight-use-package 'corfu)
(straight-use-package 'nerd-icons-corfu)
(straight-use-package 'cape)
(straight-use-package 'yasnippet-capf)

(straight-use-package 'yasnippet)

(setq mg-completion-ui 'corfu)

(use-package corfu

    :init
    (setq corfu-auto t
          corfu-auto-prefix 2
          corfu-count 14
          corfu-cycle t
          ;; Make it like `company-tng-mode'. Referenced from
          ;; doomemacs.
          corfu-preselect 'prompt
          ;; Keep the popup open when the input already matches a
          ;; candidate exactly, so a longer candidate can still be
          ;; picked, e.g. `foobar' after typing `foo'. Referenced from
          ;; doomemacs.
          corfu-on-exact-match nil
          ;; Prevent excessively long type signatures from occupying
          ;; too much screen width during completion display, e.g.,
          ;; with Python+ty.
          corfu-max-width 80
          corfu-echo-delay 0.1
          corfu-popupinfo-delay '(0.5 . 0.3)
          global-corfu-modes '((not vterm-mode eat-mode ghostel-mode) t)
          ;; Keep Corfu out of the minibuffer: vertico is used there.
          global-corfu-minibuffer nil
          completion-ignore-case t
          ;; `text-mode' adds `ispell-completion-at-point' to the
          ;; capfs by default, which errors out when no spell checker
          ;; or word list is installed.
          text-mode-ispell-word-completion nil
          ;; Do not adjust the case of the dabbrev candidates to the
          ;; case of the typed prefix, like `company-dabbrev-downcase'
          ;; set to nil.
          dabbrev-case-replace nil
          ;; Corfu comes with no key bindings for evil insert state,
          ;; evil-collection provides them (see
          ;; `evil-collection-mode-list'). The `tab-n-go' theme cycles
          ;; candidates with TAB and S-TAB, and `magic-return' makes
          ;; RET insert the selected candidate but otherwise act as a
          ;; plain newline. Together they behave like
          ;; `company-tng-mode'.
          evil-collection-corfu-key-themes '(default tab-n-go magic-return))

    ;; Fallback sources: the t in a buffer-local hook value means
    ;; "run the global list here", so these still apply.
    (setq-default completion-at-point-functions
                  (list #'cape-file #'yasnippet-capf #'cape-dabbrev))

    (mg-run-hook-once evil-insert-state-entry-hook global-corfu-mode)

    ;; Use prefix based completion styles for in-buffer completion,
    ;; just like `mg-company-completion-styles'. A hook rather than
    ;; an advice, since corfu has no single function to wrap.
    (mg-setq-on-hook corfu-mode-hook completion-styles '(basic partial-completion emacs22))

    :config

    (corfu-popupinfo-mode)
    (corfu-echo-mode)
    (advice-add #'corfu-echo--show :around #'mg-corfu-echo--one-line)

    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

    (add-hook 'corfu-mode-hook #'evil-normalize-keymaps)

    (general-define-key
     :keymaps 'corfu-map
     :states 'insert
     "C-n" #'corfu-next
     "C-p" #'corfu-previous
     "C-e" #'corfu-quit
     ;; use C-y to enter yasnippet expansion
     ;; without input of additional character.
     "C-y" #'corfu-insert)

    (general-define-key
     :keymaps 'corfu-mode-map
     :states 'insert
     ;; manually invoke the completion
     "M-i" #'completion-at-point)

    (yas-global-mode))

(use-package cape
    :init
    (setq cape-dabbrev-buffer-function #'mg-cape-dabbrev-buffers)

    ;; When eglot has no candidates, let Emacs fall through to the
    ;; remaining capfs (files, dabbrev) instead of stopping there.
    ;; This also applies to the merged capfs built on top of eglot.
    ;; Referenced from doomemacs.
    (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)

    (general-define-key
     :states 'insert
     ;; manually invoke the completion
     "M-c M-c" #'mg-complete-ctags
     "M-c M-f" #'cape-file
     "M-c M-d" #'cape-dabbrev))

(use-package yasnippet
    :init
    (setq yas-verbosity 2))

(provide 'config-completion-corfu)
;;; config-completion-corfu.el ends here
