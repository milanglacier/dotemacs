;;; my-init-vcs.el -*- lexical-binding: t; -*-

(straight-use-package 'magit)

(general-create-definer my/git-map
    :prefix "SPC g"
    :non-normal-prefix "M-SPC g"
    :prefix-map 'my/git-map)

(use-package magit
    :defer t
    :init
    (my/git-map
        :states '(normal insert visual insert)
        :keymaps 'override
        "" '(:ignore t :which-key "git")
        "g" #'magit)

    :config
    (add-to-list 'display-buffer-alist
                 '("magit:" ;; the main magit dashboard
                   ;; don't create new window if there is one magit window
                   ;; and create new tab if there isn't one magit window.
                   (display-buffer-reuse-window display-buffer-in-new-tab)))
    (general-define-key
     :states '(normal motion)
     :keymaps 'magit-status-mode-map
     "gt" #'tab-bar-switch-to-next-tab
     ;; emulate C-g
     "<escape>" #'transient-quit-one))

(provide 'my-init-vcs)
;;; my-init-vcs.el ends here
