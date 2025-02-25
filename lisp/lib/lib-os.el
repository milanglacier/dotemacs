;;; lib-os.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mg-macos-cmd-w ()
    "If there is only one tab, close emacs, otherwise close one tab"
    (interactive)
    (if (> (length (tab-bar-tabs)) 1)
            (tab-bar-close-tab)
        (evil-quit-all)))

;;;###autoload
(defun mg-tty-setup ()

    ;; Some terminals offer two different cursors: a "visible" static cursor and a
    ;; "very visible" blinking one. By default, Emacs uses the very visible cursor
    ;; and will switch back to it when Emacs is started or resumed. A nil
    ;; `visible-cursor' prevents this.
    (setq visible-cursor nil)

    ;; Enable the mouse in terminal Emacs
    (xterm-mouse-mode)
    ;; For Emacs builds with graphical support, `mwheel` is
    ;; automatically loaded at startup, even when launched with `emacs
    ;; -nw`. However, this is not the case for builds without
    ;; graphical support. To ensure mouse wheel scrolling
    ;; functionality, we explicitly load the `mwheel` module.
    (require 'mwheel)

    (evil-terminal-cursor-changer-activate)

    (xclip-mode 1)
    (message "To synchronize clipboards across remote sessions, enable `global-clipetty-mode'."))

;;;###autoload
(defun mg--server-setup ()
    (setenv "EMACS_SERVER" server-name))

(provide 'lib-os)
;;; lib-os.el ends here
