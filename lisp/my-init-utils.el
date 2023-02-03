;;; my-init-utils.el -*- lexical-binding: t; -*-

(straight-use-package 'use-package)
(straight-use-package 'general)

(setq use-package-expand-minimally t)

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

(defmacro my/run-hook-once (hook func)
    "a wrapper to run a func on a hook only once"
    (let ((func-once (gensym (concat "my/" (symbol-name func)
                                     "-" "at-" (symbol-name hook) "-" "once"))))
        `(add-hook ',hook
                   (defun ,func-once ()
                       (funcall ',func)
                       (remove-hook ',hook ',func-once)))))

(provide 'my-init-utils)
;;; my-init-utils.el ends here
