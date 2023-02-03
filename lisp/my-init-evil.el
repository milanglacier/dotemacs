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
(straight-use-package 'evil-matchit)

(use-package evil
  :defer t
  :init
  (setq evil-want-C-i-jump t
        evil-want-C-u-delete nil
        evil-want-C-u-scroll nil
        evil-want-Y-yank-to-eol t
        evil-want-C-u-delete t
        evil-want-C-w-delete t
        evil-want-keybinding nil
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
  ;; TODO: lazy load these evil modules
  ;; (reference from doomemacs)
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
  (global-evil-matchit-mode)
  (evil-collection-init)

  ;; TODO: doom hacks a lot of advices. However I don't really understand them.
  ;; (Maybe I don't face the situations where I need those advices).  Leave this
  ;; comment for the purpopse of future reminder.  If one day I met those
  ;; situations, I could still go to doom for references.

  ;; save-excursion before making indentation
  (advice-add #'evil-indent
              :around (defun my/save-excursion-before-indenting (origin-fn &rest args)
                        (save-excursion (apply origin-fn args))))

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
   "@" #'my/evil-apply-macro-line-by-line
   "Z" #'evil-snipe-S)

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
   "gd" #'xref-find-definitions
   "gr" #'xref-find-references
   "C-w gd" #'xref-find-definitions-other-window
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

  (my/define-and-bind-paren-text-object "$" "\\$" "\\$")
  (my/define-and-bind-paren-text-object "|" "|" "|")
  (my/define-and-bind-paren-text-object "=" "=" "=")
  (my/define-and-bind-paren-text-object "~" "~" "~")

  (general-define-key
   :keymaps '(evil-ex-completion-map evil-ex-search-keymap)
   "C-p" #'previous-complete-history-element
   "C-n" #'next-complete-history-element
   "C-k" #'kill-line
   "C-b" #'backward-char
   "C-f" #'forward-char
   "C-a" #'move-beginning-of-line)

  (my/open-map
    :keymaps 'override
    :states '(motion visual insert)
    ":" #'evil-command-window-ex)

  (my/toggle-map
    :keymaps 'override
    :states '(motion insert)
    "h" #'evil-ex-nohighlight)

  (general-create-definer my/window-map
    :prefix "SPC w"
    :non-normal-prefix "M-SPC w"
    :prefix-map 'my/window-map)

  (my/window-map
    :states '(motion insert)
    :keymaps 'override
    "w" #'evil-window-next
    "p" #'evil-window-mru
    "W" #'evil-window-prev
    "s" #'evil-window-split
    "v" #'evil-window-vsplit
    "h" #'evil-window-left
    "j" #'evil-window-down
    "k" #'evil-window-up
    "l" #'evil-window-right
    "q" #'evil-quit
    "o" #'delete-other-windows
    "=" #'balance-windows
    "+" #'evil-window-increase-height
    "-" #'evil-window-decrease-height
    ":" #'evil-ex
    "<" #'evil-window-increase-width
    ">" #'evil-window-increase-width
    "_" #'evil-window-set-height
    "|" #'evil-window-set-width
    "c" #'evil-window-delete
    "gd" #'xref-find-definitions-other-window
    "x" #'evil-window-exchange
    "r" #'evil-window-rotate-downwards
    "R" #'evil-window-rotate-upwards
    "H" #'evil-window-move-far-left
    "J" #'evil-window-move-far-bottom
    "K" #'evil-window-move-very-top
    "L" #'evil-window-move-far-right)

  (general-create-definer my/buffer-map
    :prefix "SPC b"
    :non-normal-prefix "M-SPC b"
    :prefix-map 'my/buffer-map)

  (my/buffer-map
    :states '(motion insert)
    :keymaps 'override
    "d" #'evil-delete-buffer
    "[" #'previous-buffer
    "]" #'next-buffer
    "s" #'consult-buffer
    "i" #'ibuffer)

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

(use-package better-jumper
  :defer t
  :config
  (general-create-definer my/jump-map
    :prefix "SPC j"
    :non-normal-prefix "M-SPC j"
    :prefix-map 'my/jump-map)
  (my/jump-map
   :states '(motion insert)
   :keymaps 'override
   "" '(:ignore t :which-key "jump")
   "o" #'better-jumper-jump-backward
   "i" #'better-jumper-jump-forward)
  )

;; adopted from
;; URL `https://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp'
;; NOTE: `make-symbol' will create new symbols even with the same name (they are different symbols)
(defmacro my/define-and-bind-paren-text-object (key start-regex end-regex)
  (let ((inner-name (gensym (concat "my-inner-" start-regex "-" end-regex "-text-obj")))
        (outer-name (gensym (concat "my-outer-" start-regex "-" end-regex "-text-obj"))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key ',inner-name)
       (define-key evil-outer-text-objects-map ,key ',outer-name))))

(defmacro my/define-and-bind-local-paren-text-object (key start-regex end-regex hook)
  (let ((inner-name (gensym (concat "my-inner-" start-regex "-" end-regex "-text-obj")))
        (outer-name (gensym (concat "my-outer-" start-regex "-" end-regex "-text-obj")))
        (lambda-name (gensym (concat "my-lambda-" start-regex "-" end-regex "-text-obj"))))
    `(add-hook ',hook
               (defun ,lambda-name ()
                 (evil-define-text-object ,inner-name (count &optional beg end type)
                   (evil-select-paren ,start-regex ,end-regex beg end type count nil))
                 (evil-define-text-object ,outer-name (count &optional beg end type)
                   (evil-select-paren ,start-regex ,end-regex beg end type count t))
                 (define-key evil-operator-state-local-map ,(concat "i" key) ',inner-name)
                 (define-key evil-operator-state-local-map ,(concat "a" key) ',outer-name)
                 (define-key evil-visual-state-local-map ,(concat "i" key) ',inner-name)
                 (define-key evil-visual-state-local-map ,(concat "a" key) ',outer-name)))))

(use-package evil-collection
  :init
  (setq evil-collection-mode-list
        '(arc-mode bm bookmark consult compilation eldoc daemons debug diff-hl
                   diff-mode dired dired-sidebar docker doc-view elisp-refs embark eglot
                   eldoc eshell eww flymake grep help helpful ibuffer imenu macrostep
                   magit-sections magit magic-todos man mu4e mu4e-conversation notmuch
                   org org-roam osx-dictionary pdf python rg ripgrep tab-bar term vertico
                   vterm wdired wgrep which-key xref xwidget)))

(provide 'my-init-evil)
;;; my-init-evil.el ends here
