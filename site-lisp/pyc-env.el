;;; pyc-env.el -*- lexical-binding: t; -*-

;; Version: 0.0.02
;; Package-Requires: ((emacs "29"))

;;; Commentary:
;; This package provides a streamlined interface for managing Python
;; virtual environments, offering support for `venv`, `conda`, and
;; `poetry`. It also includes a helper function to update the Python
;; executable path for Eglot.

;;; Code:

(defvar pyc-env-conda-activate-hook '(pyc-env-eglot-update-python-path)
    "Hook run after activating a conda environment.
The hook functions are called with one argument: the path to the
activated environment.")

(defvar pyc-env-conda-deactivate-hook nil
    "Hook run after deactivating a conda environment.")

;;;###autoload
(defun pyc-env-conda-activate (&optional path)
    "Activate a conda environment."
    (interactive)
    (pyc-env-conda-deactivate)

    (if (executable-find "conda")
            (let ((conda-info (json-parse-string (shell-command-to-string "conda info --json")
                                                 :object-type 'plist
                                                 :array-type 'list)))
                ;; read the conda environment path
                (setq path (or path (completing-read "conda env:" (plist-get conda-info :envs)))
                      ;; if path is empty, which means we want to use the base environment
                      path (if (equal path "") (plist-get conda-info :root_prefix) path)
                      ;; if the path contains trailing "bin" or "bin/", remove it
                      path (replace-regexp-in-string "/bin/?$" "" path)
                      ;; if the path contains trailing "/", remove it
                      path (replace-regexp-in-string "/$" "" path)
                      conda-current-env path
                      ;; append the path with "/bin"
                      path (concat path "/bin"))

                (setenv "PATH" (concat path path-separator (getenv "PATH")))
                ;; exec-path does not sync with $PATH on the fly.
                (add-to-list 'exec-path path)
                (setenv "CONDA_PREFIX" conda-current-env)
                (setenv "CONDA_DEFAULT_ENV" (file-name-nondirectory conda-current-env))
                (setenv "CONDA_PROMPT_MODIFIER" (concat "(" (file-name-nondirectory conda-current-env) ") "))
                (setenv "CONDA_SHLVL" "1")
                (run-hook-with-args 'pyc-env-conda-activate-hook conda-current-env)
                (message "Activating conda environment: %s" path))
        (message "conda not found")))

;;;###autoload
(defun pyc-env-conda-deactivate ()
    "Deactivate all the conda environments, including the base environment."
    (interactive)
    (if (executable-find "conda")
            (let* ((paths (split-string (getenv "PATH") path-separator))
                   (conda-info (json-parse-string (shell-command-to-string "conda info --json")
                                                  :object-type 'plist
                                                  :array-type 'list))
                   (conda-current-env (plist-get conda-info :default_prefix)))

                (setq paths (delete (concat conda-current-env "/bin") paths)
                      exec-path (delete (concat conda-current-env "/bin") exec-path))
                (setenv "PATH" (string-join paths path-separator))
                (setenv "CONDA_PREFIX" nil)
                (setenv "CONDA_DEFAULT_ENV" nil)
                (setenv "CONDA_SHLVL" "0")
                (setenv "CONDA_PROMPT_MODIFIER" nil)
                (run-hooks 'pyc-env-conda-deactivate-hook)
                (message "Conda environment deactivated."))
        (message "conda not found")))



(defvar pyc-env-venv-activate-hook '(pyc-env-eglot-update-python-path)
    "Hook run after activating a python virtual environment.
The hook functions are called with one argument: the path to the activated environment.")

(defvar pyc-env-venv-deactivate-hook nil
    "Hook run after deactivating a python virtual environment.")

;;;###autoload
(defun pyc-env-venv-activate (&optional path)
    "Activate a python virtual environment."
    (interactive "Dselect a python venv: ")

    (pyc-env-venv-deactivate)

    ;; if the path contains trailing "bin" or "bin/", remove it
    (let (pyvenv-current-env)

        (setq path (expand-file-name path)
              path (replace-regexp-in-string "/bin/?$" "" path)
              ;; if the path contains trailing "/", remove it
              path (replace-regexp-in-string "/$" "" path)
              pyvenv-current-env path
              ;; append the path with "/bin"
              path (concat path "/bin"))

        (setenv "PATH" (concat path path-separator (getenv "PATH")))
        (add-to-list 'exec-path path)
        (setenv "VIRTUAL_ENV" pyvenv-current-env)
        (run-hook-with-args 'pyc-env-venv-activate-hook pyvenv-current-env)
        (message "Activating python venv: %s" path)))

;;;###autoload
(defun pyc-env-venv-deactivate ()
    "Deactivate the current python virtual environment."
    (interactive)
    (when-let* ((pyvenv-current-env (and (not (equal (getenv "VIRTUAL_ENV") ""))
                                         (getenv "VIRTUAL_ENV"))))
        ;; split the PATH
        (let ((paths (split-string (getenv "PATH") path-separator)))
            ;; remove the path to the current python venv environment
            (setq paths (delete (concat pyvenv-current-env "/bin") paths)
                  exec-path (delete (concat pyvenv-current-env "/bin") exec-path))
            ;; set the PATH
            (setenv "PATH" (string-join paths path-separator))
            (setenv "VIRTUAL_ENV" nil)
            (run-hooks 'pyc-env-venv-deactivate-hook))))


;;;###autoload
(defun pyc-env-poetry-activate (path)
    "Activate a poetry virtual environment.
If only one environment exists, activate it directly. Otherwise, prompt for selection."
    (interactive
     (let ((envs
            (condition-case error
                    (seq-filter (lambda (x) (not (equal x "")))
                                (process-lines "poetry" "env" "list" "--full-path"))
                (error (error "current project is not a poetry project or poetry is not installed!")))))
         (list
          (if (length= envs 1) (car envs)
              (completing-read "Select a poetry venv: " envs nil t)))))
    (pyc-env-venv-activate (replace-regexp-in-string " (Activated)$" "" path)))

;;;###autoload
(defun pyc-env-poetry-deactivate ()
    "Deactivate the current poetry virtual environment."
    (interactive)
    (pyc-env-venv-deactivate))

(defmacro pyc-env--setf-nested-plist (place val &rest attributes)
    "Set a PLIST's nested ATTRIBUTES to VAL.
Example usage: (pyc-env-setf-nested-plist a-plist \"hello\" :level-1 :level-2)."
    (if (null attributes)
            (error "pyc-env-setf-nested-plist requires at least one attribute key"))
    (let ((access-form place))
        (dolist (attr attributes)
            (setq access-form `(plist-get ,access-form ',attr)))
        `(setf ,access-form ,val)))

(defun pyc-env-eglot-update-python-path (path)
    "Updates the Python path used by the Eglot server."
    (when-let* ((current-server (eglot-current-server))
                (server-info (eglot--server-info current-server))
                (is-based-pyright (equal (plist-get server-info :name)
                                         "basedpyright"))
                (config (copy-tree eglot-workspace-configuration)))
        (pyc-env--setf-nested-plist config
                                    (concat path "/bin/python3")
                                    :python
                                    :pythonPath)
        ;; HACK: Eglot uses dir-local variables for
        ;; `eglot-workspace-configuration'.  To programmatically apply
        ;; a specific configuration, this function temporarily advises
        ;; `eglot--workspace-configuration-plist` to return a custom
        ;; settings plist.
        (defalias #'pyc-env-eglot-set-workspace-configuration
            (lambda (&rest _)
                config))
        (advice-add #'eglot--workspace-configuration-plist :around #'pyc-env-eglot-set-workspace-configuration)

        (eglot-signal-didChangeConfiguration current-server)
        ;; `eglot--workspace-configuration-plist' may be invoked
        ;; multiple times by Eglot when settings change. The advice is
        ;; removed via an idle timer to ensure all such invocations
        ;; use the updated configuration before cleanup.
        (run-with-idle-timer
         1 nil
         #'advice-remove #'eglot--workspace-configuration-plist #'pyc-env-eglot-set-workspace-configuration)))


(provide 'pyc-env)
