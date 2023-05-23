;;; my-init-utils.el -*- lexical-binding: t; -*-

(straight-use-package 'use-package)
(straight-use-package 'general)

(general-create-definer my/leader
    ;; :prefix my-leader
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

(general-create-definer my/localleader
    ;; :prefix my-leader
    :prefix "SPC SPC"
    :non-normal-prefix "M-SPC M-SPC")

(my/leader
    "SPC" '(:ignore t :which-key "Local Leader")
    "o" '(:ignore t :which-key "Open")
    "t" '(:ignore t :which-key "toggle"))

(general-create-definer my/open-map
    :prefix "SPC o"
    :non-normal-prefix "M-SPC o"
    :prefix-map 'my/open-map)

(general-create-definer my/toggle-map
    :prefix "SPC t"
    :non-normal-prefix "M-SPC t"
    :prefix-map 'my/toggle-map)

(add-hook 'emacs-startup-hook #'my:load-packages-incrementally-setup)

(provide 'my-init-utils)
;;; my-init-utils.el ends here
