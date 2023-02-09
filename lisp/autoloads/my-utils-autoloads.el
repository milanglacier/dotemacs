;;; my-utils-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defmacro my/run-hook-once (hook func &rest args)
    "a wrapper to run a func on a hook only once"
    (let ((func-once (gensym (concat "my/" (symbol-name func)
                                     "-" "at-" (symbol-name hook) "-" "once"))))
        `(add-hook ',hook
                   (defun ,func-once ()
                       (funcall ',func)
                       (remove-hook ',hook ',func-once)) ,@args)))

(provide 'my-utils-autoloads)
;;; my-utils-autoloads ends here
