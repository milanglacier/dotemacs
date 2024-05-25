;;; my-os-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/macos-cmd-w ()
    "If there is only one tab, close emacs, otherwise close one tab"
    (interactive)
    (if (> (length (tab-bar-tabs)) 1)
            (tab-bar-close-tab)
        (evil-quit-all)))

;;;###autoload
(defun my/tty-setup ()

    ;; Some terminals offer two different cursors: a "visible" static cursor and a
    ;; "very visible" blinking one. By default, Emacs uses the very visible cursor
    ;; and will switch back to it when Emacs is started or resumed. A nil
    ;; `visible-cursor' prevents this.
    (setq visible-cursor nil)

    ;; Enable the mouse in terminal Emacs
    (xterm-mouse-mode)
    (evil-terminal-cursor-changer-activate)
    ;; the following keys correspond to touchpad gestures.
    (general-define-key [mouse-4] #'scroll-down-line
                        [mouse-6] #'scroll-down-line
                        [mouse-5] #'scroll-up-line
                        [mouse-7] #'scroll-up-line)
    (menu-bar-mode -1)
    ;; BUG: in tty while menu-bar is not displayed, however it is
    ;; still there, that is, when you try to use mouse to click the
    ;; region, they pops up something which suggests its existence.
    ;; Have to turn off it again.
    (xclip-mode 1))

;;;###autoload
(defun my:server-setup ()
    (setenv "EMACS_SERVER" server-name))

(provide 'my-os-autoloads)
;;; my-os-autoloads.el ends here
