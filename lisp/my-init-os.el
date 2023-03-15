;;; my-init-os.el -*- lexical-binding: t; -*-

(straight-use-package 'xclip)
(straight-use-package 'evil-terminal-cursor-changer)

(when IS-MAC
    ;; Use spotlight search backend as a default for M-x locate (and helm/ivy
    ;; variants thereof), since it requires no additional setup.
    (setq locate-command "mdfind"
          ;; Visit files opened outside of Emacs in existing frame, not a new one
          ns-pop-up-frames nil)

    (general-define-key
     "C-s-f" #'toggle-frame-fullscreen
     "s-w" #'my/macos-cmd-w
     "s-t" #'tab-bar-new-tab
     "s-1" #'my/tab-bar-go-to-tab-1
     "s-2" #'my/tab-bar-go-to-tab-2
     "s-3" #'my/tab-bar-go-to-tab-3
     "s-4" #'my/tab-bar-go-to-tab-4
     "s-5" #'my/tab-bar-go-to-tab-5
     "s-6" #'my/tab-bar-go-to-tab-6
     "s-7" #'my/tab-bar-go-to-tab-7
     "s-8" #'my/tab-bar-go-to-tab-8
     "s-9" #'my/tab-bar-go-to-tab-9)
    )

(when IS-LINUX
    (general-define-key
     "C-S-c" #'evil-yank
     "C-S-v" #'evil-paste-after))

(add-hook 'tty-setup-hook #'my/tty-setup)

(provide 'my-init-os)
;;; my-init-os.el ends here
