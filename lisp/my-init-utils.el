;;; my-init-utils.el -*- lexical-binding: t; -*-

(straight-use-package 'use-package)
(straight-use-package 'general)

(setq use-package-expand-minimally t)

(general-create-definer my/leader
  ;; :prefix my-leader
  :prefix "SPC")

(general-create-definer my/localleader
  ;; :prefix my-local-leader
  :prefix "SPC SPC")

(provide 'my-init-utils)
;;; my-init-utils.el ends here
