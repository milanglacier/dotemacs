;;; my-utils-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/update-all-autoloads ()
    (interactive)
    (when (not (file-exists-p my/autoloads-file))
        (with-current-buffer (find-file-noselect
                              my/autoloads-file)
            (save-buffer)))
    (make-directory-autoloads my/autoloads-dir my/autoloads-file))

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

;;;###autoload
(defmacro my/turn-off-mode (mode)
    "Create a function to turn off MODE. Useful for attaching on some
hooks that will turn off MODE locally."
    (let ((func (intern (concat "my/turn-off-"
                                (symbol-name mode)))))
        `(if (fboundp #',func)
                 #',func
             (defun ,func ()
                 (,mode -1)))))

;;;###autoload
(defmacro my/setq-locally (var val)
    "Create a function to set VAR to VAL locally. Useful for attaching
on some hooks that will change the variable locally.

Use `my/setq-locally' when you want to set VAR to a simple VAL in many
modes.  Use `my/setq-on-hook' when you want to set VAR to a complex
VAL in very few modes.  Why don't I just directly use `(add-hook
'foo-hook (lambda () (FORM)))'?  Because when you try to
\\[describe-variable] `foo-hook RET', you will find those lambda
function will be unreadable. And using a named function in a hook
makes those variable displayed much more nicely.  This is very helpful
for debugging purpose if you want to examine a hook value."
    (let ((func (intern (concat "my/set-"
                                (symbol-name var)
                                "-to-"
                                (prin1-to-string val)
                                "-locally"))))
        `(if (fboundp #',func)
                 #',func
             (defun ,func ()
                 (setq-local ,var ,val)))))

;;;###autoload
(defmacro my/setq-on-hook (hook var val)
    "Create a function to set VAR to VAL on a HOOK.

Use `my/setq-locally' when you want to set VAR to a simple VAL in many
modes.  Use `my/setq-on-hook' when you want to set VAR to a complex
VAL in only one mode.  Why don't I just directly use `(add-hook
'foo-hook (lambda () (FORM)))'?  Because when you try to
\\[describe-variable] `foo-hook RET', you will find those lambda
function will be unreadable. And using a named function in a hook
makes those variable displayed much more nicely.  This is very helpful
for debugging purpose if you want to examine a hook value."
    (let ((func (intern (concat "my/set-"
                                (symbol-name var)
                                "-on-"
                                (symbol-name hook)))))
        `(add-hook ',hook (if (fboundp #',func)
                                  #',func
                              (defun ,func ()
                                  (setq-local ,var ,val))))))

(provide 'my-utils-autoloads)
;;; my-utils-autoloads ends here
