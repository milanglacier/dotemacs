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
        "g" #'magit)

    :config
    (add-to-list 'display-buffer-alist
                 '("magit:" ;; the main magit dashboard
                   (display-buffer-in-new-tab))))

(provide 'my-init-vcs)
;;; my-init-vcs.el ends here
