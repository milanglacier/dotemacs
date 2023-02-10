;;; my-utils-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defmacro my/run-hook-once (hook func &rest args)
    "a wrapper to run a func on a hook only once"
    (let ((func-once (gensym (concat "my/" (symbol-name func)
                                     "-" "at-" (symbol-name hook) "-" "once"))))
        `(add-hook ',hook
                   (defun ,func-once ()
                       (funcall #',func)
                       (remove-hook ',hook #',func-once)) ,@args)))

;;;###autoload
(defmacro my/advise-at-once (func advice where &rest props)
    "a wrapper to advise a func only once"
    (let ((advice-once (gensym
                        (concat "my/" (symbol-name advice)
                                (symbol-name where) "-" (symbol-name func) "-" "once"))))
        `(advice-add #',func ,where
                     (defun ,advice-once (&rest _)
                         (funcall #',advice)
                         (advice-remove #',func #',advice-once)) ,@props)))

(provide 'my-utils-autoloads)
;;; my-utils-autoloads ends here
