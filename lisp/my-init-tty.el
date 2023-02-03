;;; my-init-tty.el -*- lexical-binding: t; -*-

(straight-use-package 'xclip)
(straight-use-package 'evil-terminal-cursor-changer)

;; Some terminals offer two different cursors: a "visible" static cursor and a
;; "very visible" blinking one. By default, Emacs uses the very visible cursor
;; and will switch back to it when Emacs is started or resumed. A nil
;; `visible-cursor' prevents this.
(setq visible-cursor nil)
(blink-cursor-mode -1)

;; to seperate tab from C-i in GUI mode
;; since C-i and TAB is indistinguishable in GUI mode
(general-define-key
 :keymaps 'function-key-map
 "<tab>" nil)

(defun my/tty-setup ()
  (xterm-mouse-mode)
  (evil-terminal-cursor-changer-activate)
  ;; Enable the mouse in terminal Emacs
  (general-define-key [mouse-4] #'scroll-down-line
                      [mouse-5] #'scroll-up-line)
  (xclip-mode 1))

(add-hook 'tty-setup-hook #'my/tty-setup)

(provide 'my-init-tty)
;;; my-tty.el ends here
