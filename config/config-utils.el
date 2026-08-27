;;; config-utils.el -*- lexical-binding: t; -*-

(straight-use-package '(use-package :type built-in))
(straight-use-package 'general)

(general-create-definer mg-leader
    ;; :prefix mg-leader
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

(general-create-definer mg-localleader
    ;; :prefix mg-leader
    :prefix "SPC SPC"
    :non-normal-prefix "M-SPC M-SPC")

(mg-leader
    "SPC" '(:ignore t :which-key "Local Leader")
    "o" '(:ignore t :which-key "Open")
    "t" '(:ignore t :which-key "toggle"))

(general-create-definer mg-open-map
    :prefix "SPC o"
    :non-normal-prefix "M-SPC o"
    :prefix-map 'mg-open-map)

(general-create-definer mg-toggle-map
    :prefix "SPC t"
    :non-normal-prefix "M-SPC t"
    :prefix-map 'mg-toggle-map)

(add-hook 'emacs-startup-hook #'mg--load-packages-incrementally-setup)

(provide 'config-utils)
;;; config-utils.el ends here
