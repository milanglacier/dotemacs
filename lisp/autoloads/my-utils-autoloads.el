;;; my-utils-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/update-all-autoloads (&optional autoloads-dir autoloads-file)
    "Update all autoloads in the AUTOLOADS-DIR into the AUTOLOADS-FILE.
If AUTOLOADS-DIR is nil, use `my/autoloads-dir'. If AUTOLOADS-FILE is
nil, use `my/autoloads-file'."
    (interactive)
    (let ((autoloads-dir (or autoloads-dir my/autoloads-dir))
          (autoloads-file (or autoloads-file my/autoloads-file)))
        (when (not (file-exists-p autoloads-file))
            (with-current-buffer (find-file-noselect
                                  autoloads-file)
                (save-buffer)))
        (loaddefs-generate autoloads-dir autoloads-file nil nil nil t)))

;;;###autoload
(defun my/update-site-lisp-autoloads ()
    (interactive)
        (my/update-all-autoloads my/site-lisp-dir my/site-lisp-autoloads-file))

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
    "Create a function that sets a local value to a variable (VAR)
called VAL. This function is particularly useful for setting variables
locally in certain hooks.

For setting a simple VAL to VAR in multiple modes, use
`my/setq-locally'. In case you want to set a complex VAL to VAR in a
single mode, use `my/setq-on-hook'. You might wonder why you shouldn't
simply use (add-hook 'foo-hook (lambda () FORM)? This is because, upon
running \\[describe-variable] `foo-hook RET', you'll find the lambda
functions unreadable. Using a named function in a hook, however, makes
the hook more elegantly described, which proves to be useful for
debugging purposes if you desire to scrutinize a hook value."
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
    "Create a function that sets VAR to VAL on a HOOK.

If you want to set VAR to a simple VAL in multiple modes, use
`my/setq-locally'. However, if you want to set VAR to a complex VAL in
only one mode, use `my/setq-on-hook'. Why not directly use (add-hook
'foo-hook (lambda () FORM))'? Well, when you try to describe the
variable with \\[describe-variable] `foo-hook RET', those lambda
functions become unreadable. Using a named function in a hook results
in much nicer description of the hook. This is particularly helpful
for debugging purposes when you want to examine a hook value."
    (let ((func (intern (concat "my/set-"
                                (symbol-name var)
                                "-on-"
                                (symbol-name hook)))))
        `(add-hook ',hook (if (fboundp #',func)
                                  #',func
                              (defun ,func ()
                                  (setq-local ,var ,val))))))

(defvar my$load-incrementally-packages ()
    "packages to be loaded incrementally after startup")
(defvar my$load-incrementally-packages-first-time 2
    "time to load packages incrementally for the first time")
(defvar my$load-incrementally-packages-idle-time 0.75
    "idle time to load packages incrementally")

(defun my:load-packages-incrementally (pkgs)
    "load package from PKGS incrementally"
    (let ((gc-cons-threshold most-positive-fixnum)
          (pkg (car pkgs))
          (rest-pkgs (cdr pkgs)))
        (when pkgs
            (if (featurep pkg)
                    (my:load-packages-incrementally rest-pkgs)
                (progn
                    (require pkg)
                    (run-with-idle-timer
                     my$load-incrementally-packages-idle-time nil
                     #'my:load-packages-incrementally rest-pkgs))))))

;;;###autoload
(defun my:load-packages-incrementally-setup ()
    "Set up a idle timer to start idly load packages."
    (run-with-idle-timer
     my$load-incrementally-packages-first-time nil
     #'my:load-packages-incrementally my$load-incrementally-packages))

(defvar my$function-predicate-blocklist ()
    "An alist wherein elements are organized as follows: (function
predicate-1 predicate-2, ...). When either predicate-1, predicate-2,
etc., evaluates to true, the function will not be invoked. This
becomes beneficial when used with a hook. For instance, you may wish
to activate `eglot' while using `org-mode', but prefer not to enable
`eglot' when `org-msg-edit-mode' is activated.")

(defmacro my%call-func-respect-blocklist (func)
    "Calling FUNC only when all the forms associated with FUNC in
`my$function-predicate-blocklist' evalutes to nil."
    (let ((func-with-blocklist
           (intern (format "my*%s-with-blocklist" (symbol-name func)))))
        `(if (fboundp #',func-with-blocklist)
                 #',func-with-blocklist
             (defun ,func-with-blocklist ()
                 (let* ((blocklist (alist-get ',func my$function-predicate-blocklist)))
                     (unless (eval `(or ,@blocklist))
                         (,func)))))))

(provide 'my-utils-autoloads)
;;; my-utils-autoloads ends here
