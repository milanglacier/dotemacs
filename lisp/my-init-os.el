;;; my-init-os.el -*- lexical-binding: t; -*-

(when IS-MAC
    ;; Use spotlight search backend as a default for M-x locate (and helm/ivy
    ;; variants thereof), since it requires no additional setup.
    (setq locate-command "mdfind"
          ;; Visit files opened outside of Emacs in existing frame, not a new one
          ns-pop-up-frames nil)

    (defun my/macos-cmd-w ()
        "If there is only one tab, close emacs, otherwise close one tab"
        (interactive)
        (if (> (length (tab-bar-tabs)) 1)
                (tab-bar-close-tab)
            (evil-quit-all)))

    (general-define-key
     "C-s-f" #'toggle-frame-fullscreen
     "s-w" #'my/macos-cmd-w
     "s-t" #'tab-bar-new-tab)
    )

(provide 'my-init-os)
;;; my-init-evil.el ends here
