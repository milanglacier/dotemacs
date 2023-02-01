;;; my-init-evil.el -*- lexical-binding: t; -*-

(straight-use-package 'evil)
(straight-use-package 'evil-goggles)
(straight-use-package 'evil-escape)
(straight-use-package 'better-jumper)
(straight-use-package 'evil-anzu)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-embrace)
(straight-use-package 'evil-snipe)
(straight-use-package 'evil-traces)
(straight-use-package 'evil-exchange)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'evil-visualstar)
(straight-use-package 'evil-replace-with-register)
(straight-use-package 'evil-args)
(straight-use-package 'exato)
(straight-use-package 'evil-lion)
(straight-use-package 'evil-vimish-fold)
(straight-use-package 'evil-indent-plus)
(straight-use-package 'expand-region)
(straight-use-package 'evil-collection)

(use-package evil
  :defer t
  :init
  (setq evil-want-C-i-jump t
	evil-want-C-u-delete nil
	evil-want-C-u-scroll nil
	evil-want-Y-yank-to-eol t
	evil-want-C-u-delete t
	evil-want-C-w-delete t
	evil-want-abbrev-expand-on-insert-exit t
	evil-visual-update-x-selection-p nil
	evil-mode-line-format nil
	evil-ex-search-vim-style-regexp t
	evil-ex-visual-char-range t
	evil-mode-line-format nil
	evil-normal-state-cursor 'box
	evil-insert-state-cursor 'bar
	evil-emacs-state-cursor 'box
	evil-visual-state-cursor 'hollow
	evil-ex-interactive-search-highlight 'selected-window
	;; It's infuriating that innocuous "beginning of line" or "end of line"
	;; errors will abort macros, so suppress them:
	evil-kbd-macro-suppress-motion-error t
	evil-undo-system 'undo-redo
	evil-ex-hl-update-delay 0.1)

  ;; at the very start of the hook
  (add-hook 'after-init-hook #'evil-mode -90)

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-goggles-mode)
  (evil-escape-mode)
  (global-evil-surround-mode)
  (evil-embrace-enable-evil-surround-integration)
  (evil-traces-mode)
  (evil-exchange-install)
  (evil-snipe-mode)
  (evil-snipe-override-mode)
  (global-evil-visualstar-mode)
  (better-jumper-mode)
  (global-evil-vimish-fold-mode)
  (global-anzu-mode)

  ;; TODO: when wgrep is configured, copy #'+evil-delete from doomemacs.

  ;; TODO: doom hacks a lot of advices. However I don't really understand them.
  ;; (Maybe I don't face the situations where I need those advices).  Leave this
  ;; comment for the purpopse of future reminder.  If one day I met those
  ;; situations, I could still go to doom for references.

  ;; copied from doomemacs
  (evil-define-operator my/evil-apply-macro-line-by-line (beg end)
    "Apply macro to each line."
    :move-point nil
    (interactive "<r>")
    (let ((register (or evil-this-register (read-char)))
	  macro)
      (cond ((or (and (eq register ?@) (eq evil-last-register ?:))
		 (eq register ?:))
	     (setq macro (lambda () (evil-ex-repeat nil))
		   evil-last-register ?:))
	    ((eq register ?@)
	     (unless evil-last-register
	       (user-error "No previously executed keyboard macro."))
	     (setq macro (evil-get-register evil-last-register t)))
	    ((setq macro (evil-get-register register t)
		   evil-last-register register)))
      (unless macro
	(user-error "No macro recorded in %c register" register))
      (evil-change-state 'normal)
      (evil-with-single-undo
	(let ((lines (count-lines beg end)))
	  (message "Applied macro in %c register %d times" register lines)
	  (apply-macro-to-region-lines beg end macro)
	  (message "Applied macro in %c register %d times...DONE" register lines)))))

  (general-define-key
   :states 'visual
   "@" #'my/evil-apply-macro-line-by-line)

  (general-define-key
   :states 'motion
   [remap evil-jump-backward] #'better-jumper-jump-backward
   [remap evil-jump-forward] #'better-jumper-jump-forward
   "[b" #'previous-buffer
   "]b" #'next-buffer
   "]a" #'evil-forward-arg
   "[a" #'evil-backward-arg)

  (general-define-key
   :states '(normal visual)
   "gc" #'evilnc-comment-operator
   "ga" #'evil-lion-left
   "gA" #'evil-lion-right
   "RET" #'er/expand-region
   "gs" #'evil-replace-with-register
   "g@" #'my/evil-apply-macro-line-by-line)

  (general-define-key
   :states 'insert
   "C-a" #'move-beginning-of-line
   "C-e" #'move-end-of-line
   "C-p" #'previous-line
   "C-n" #'next-line
   "C-k" #'kill-line)

  (general-define-key
   :keymaps 'in
   ;; TODO: configure emacs-lisp mode to use space as args delimiter.
   "a" #'evil-inner-arg
   "#" #'evilnc-inner-comment
   "i" #'evil-indent-plus-i-indent
   "j" #'evil-indent-plus-i-indent-up-down
   "k" #'evil-indent-plus-i-indent-up
   "x" #'evil-inner-xml-attr)

  (general-define-key
   :keymaps 'out
   "a" #'evil-outer-arg
   "#" #'evilnc-outer-comment
   "i" #'evil-indent-plus-i-indent
   "j" #'evil-indent-plus-i-indent-up-down
   "k" #'evil-indent-plus-i-indent-up
   "x" #'evil-outer-xml-attr)

  (general-define-key
   :keymaps '(evil-ex-completion-map evil-ex-search-keymap)
   "C-p" #'previous-complete-history-element
   "C-n" #'next-complete-history-element)

  )

(use-package evil-goggles
  :defer t
  :init
  (setq evil-goggles-duration 0.5
        evil-goggles-pulse nil
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil))

(use-package evil-escape
  :defer t
  :init
  (setq evil-escape-key-sequence "jk"))

(use-package evil-embrace
  :commands (embrace-add-pair embrace-add-pair-regexp)
  :hook ((LaTeX-mode . embrace-LaTeX-mode-hook)
         (org-mode . embrace-org-mode-hook)
         (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)))

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :config
  (general-define-key
   [remap comment-line] #'evilnc-comment-or-uncomment-lines))

(use-package evil-snipe
  :defer t
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'buffer
        evil-snipe-use-vim-sneak-bindings t
        evil-snipe-repeat-keys nil
        evil-snipe-char-fold t)
  )

(use-package evil-vimish-fold
  :defer t
  :init
  (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode)))

(use-package evil-anzu
  :after anzu)

(provide 'my-init-evil)
;;; my-init-evil.el ends here
