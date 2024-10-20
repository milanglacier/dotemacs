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
    (evil-terminal-cursor-changer-activate)
    ;; the following keys correspond to touchpad gestures.
    (general-define-key [mouse-4] #'scroll-down-line
                        [mouse-6] #'scroll-down-line
                        [mouse-5] #'scroll-up-line
                        [mouse-7] #'scroll-up-line)

    (xclip-mode 1))

;;;###autoload
(defun mg--server-setup ()
    (setenv "EMACS_SERVER" server-name))

(provide 'lib-os)
;;; lib-os.el ends here
