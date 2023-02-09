;;; my-tty-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/tty-setup ()

    ;; Some terminals offer two different cursors: a "visible" static cursor and a
    ;; "very visible" blinking one. By default, Emacs uses the very visible cursor
    ;; and will switch back to it when Emacs is started or resumed. A nil
    ;; `visible-cursor' prevents this.
    (setq visible-cursor nil)
    (blink-cursor-mode -1)

    (xterm-mouse-mode)
    (evil-terminal-cursor-changer-activate)
    ;; Enable the mouse in terminal Emacs
    (general-define-key [mouse-4] #'scroll-down-line
                        [mouse-5] #'scroll-up-line)
    (menu-bar-mode -1)
    ;; BUG: in tty while menu-bar is not displayed, however it is
    ;; still there, that is, when you try to use mouse to click the
    ;; region, they pops up something which suggests its existence.
    ;; Have to turn off it again.
    (xclip-mode 1))

(provide 'my-tty-autoloads)
;;; my-tty-autoloads ends here
