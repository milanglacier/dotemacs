;;; lib-utils.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mg-update-all-autoloads (&optional autoloads-dir autoloads-file)
    "Update all autoloads in the AUTOLOADS-DIR into the AUTOLOADS-FILE.
If AUTOLOADS-DIR is nil, use `mg-autoloads-dir'. If AUTOLOADS-FILE is
nil, use `mg-autoloads-file'."
    (interactive)
    (let ((autoloads-dir (or autoloads-dir mg-lib-dir))
          (autoloads-file (or autoloads-file mg-autoloads-file)))
        (when (not (file-exists-p autoloads-file))
            (with-current-buffer (find-file-noselect
                                  autoloads-file)
                (save-buffer)))
        (loaddefs-generate autoloads-dir autoloads-file nil nil nil t)))

;;;###autoload
(defun mg-update-site-lisp-autoloads ()
    (interactive)
    (mg-update-all-autoloads mg-site-lisp-dirs mg-site-lisp-autoloads-file))

;;;###autoload
(defmacro mg-run-hook-once (hooks func &rest args)
    "a wrapper to run a function (can be named or lambda) on a hook or
a list of hooks only once.  When HOOKS is a list, only run FUNC once
on the first hook. The rest hook will not run FUNC.

ARGS is a plist which takes the following keys:

:func-name a string which is the name of FUNC. If FUNC is a lambda,
must be provided. Can be omitted if FUNC is a symbol.

:func-args a list which is the rest args passed to FUNC."
    (when (not (or (symbolp func)
                   (plist-get args :func-name)))
        (error "when func is not a symbol, must provide :func-name"))
    (let* ((hooks
            (if (listp hooks) hooks (list hooks)))
           (func-name (or (plist-get args :func-name) (symbol-name func)))
           (func-args (plist-get args :func-args))
           (func-once (gensym (format "mg-%s-at-%s-once"
                                      func-name
                                      (mapconcat #'symbol-name hooks)))))
        `(progn
             (defun ,func-once ()
                 (funcall #',func ,@func-args)
                 ,@(mapcar
                    (lambda (hook) `(remove-hook ',hook #',func-once))
                    hooks))
             ,@(mapcar
                (lambda (hook) `(add-hook ',hook #',func-once))
                hooks))))

;;;###autoload
(defmacro mg-advise-at-once (func advice where &rest props)
    "a wrapper to advise a func only once"
    (let ((advice-once (gensym
                        (concat "mg-" (symbol-name advice)
                                (symbol-name where) "-" (symbol-name func) "-" "once"))))
        `(advice-add #',func ,where
                     (defun ,advice-once (&rest _)
                         (prog1
                                 (funcall #',advice)
                             (advice-remove #',func #',advice-once)))
                     ,@props)))

;;;###autoload
(defmacro mg-turn-off-mode (mode)
    "Create a function to turn off MODE. Useful for attaching on some
hooks that will turn off MODE locally."
    (let ((func (intern (concat "mg-turn-off-"
                                (symbol-name mode)))))
        `(if (fboundp #',func)
                 #',func
             (defun ,func ()
                 (,mode -1)))))

;;;###autoload
(defmacro mg-setq-locally (var val)
    "Create a function that sets a local value to a variable (VAR)
called VAL. This function is particularly useful for setting variables
locally in certain hooks.

For setting a simple VAL to VAR in multiple modes, use
`mg-setq-locally'. In case you want to set a complex VAL to VAR in a
single mode, use `mg-setq-on-hook'. You might wonder why you shouldn't
simply use (add-hook 'foo-hook (lambda () FORM)? This is because, upon
running \\[describe-variable] `foo-hook RET', you'll find the lambda
functions unreadable. Using a named function in a hook, however, makes
the hook more elegantly described, which proves to be useful for
debugging purposes if you desire to scrutinize a hook value."
    (let ((func (intern (concat "mg-set-"
                                (symbol-name var)
                                "-to-"
                                (prin1-to-string val)
                                "-locally"))))
        `(if (fboundp #',func)
                 #',func
             (defun ,func ()
                 (setq-local ,var ,val)))))

;;;###autoload
(defmacro mg-setq-on-hook (hook var val)
    "Create a function that sets VAR to VAL on a HOOK.

If you want to set VAR to a simple VAL in multiple modes, use
`mg-setq-locally'. However, if you want to set VAR to a complex VAL in
only one mode, use `mg-setq-on-hook'. Why not directly use (add-hook
'foo-hook (lambda () FORM))'? Well, when you try to describe the
variable with \\[describe-variable] `foo-hook RET', those lambda
functions become unreadable. Using a named function in a hook results
in much nicer description of the hook. This is particularly helpful
for debugging purposes when you want to examine a hook value."
    (let ((func (intern (concat "mg-set-"
                                (symbol-name var)
                                "-on-"
                                (symbol-name hook)))))
        `(add-hook ',hook (if (fboundp #',func)
                                  #',func
                              (defun ,func ()
                                  (setq-local ,var ,val))))))

(defvar mg-load-incrementally-packages ()
    "packages to be loaded incrementally after startup")
(defvar mg-load-incrementally-packages-first-time 2
    "time to load packages incrementally for the first time")
(defvar mg-load-incrementally-packages-idle-time 0.75
    "idle time to load packages incrementally")

(defun mg--load-packages-incrementally (pkgs)
    "load package from PKGS incrementally"
    (let ((gc-cons-threshold most-positive-fixnum)
          (pkg (car pkgs))
          (rest-pkgs (cdr pkgs)))
        (when pkgs
            (if (featurep pkg)
                    (mg--load-packages-incrementally rest-pkgs)
                (progn
                    (require pkg)
                    (run-with-idle-timer
                     mg-load-incrementally-packages-idle-time nil
                     #'mg--load-packages-incrementally rest-pkgs))))))

;;;###autoload
(defun mg--load-packages-incrementally-setup ()
    "Set up a idle timer to start idly load packages."
    (run-with-idle-timer
     mg-load-incrementally-packages-first-time nil
     #'mg--load-packages-incrementally mg-load-incrementally-packages))

(defvar mg-function-predicate-blocklist ()
    "An alist wherein elements are organized as follows: (function
predicate-1 predicate-2, ...). When either predicate-1, predicate-2,
etc., evaluates to true, the function will not be invoked. This
becomes beneficial when used with a hook. For instance, you may wish
to activate `eglot' while using `org-mode', but prefer not to enable
`eglot' when `org-msg-edit-mode' is activated.")

(defmacro mg--call-func-respect-blocklist (func)
    "Calling FUNC only when all the forms associated with FUNC in
`mg-function-predicate-blocklist' evalutes to nil."
    (let ((func-with-blocklist
           (intern (format "mg-%s-with-blocklist" (symbol-name func)))))
        `(if (fboundp #',func-with-blocklist)
                 #',func-with-blocklist
             (defun ,func-with-blocklist ()
                 (let* ((blocklist (alist-get ',func mg-function-predicate-blocklist)))
                     (unless (eval `(or ,@blocklist))
                         (,func)))))))

(provide 'lib-utils)
;;; lib-utils ends here
