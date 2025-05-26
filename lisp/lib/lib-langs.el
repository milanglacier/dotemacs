;;; lib-langs.el -*- lexical-binding: t; -*-

;;;###autoload (autoload #'mg-send-region-to-ess "lib-langs" nil t)
(evil-define-operator mg-send-region-to-ess (beg end)
    "This operator sends the region (either motion or text objects) to ess REPL"
    ;; t means don't echo the region in the ess REPL buffer
    (ess-eval-region beg end t))

;;;###autoload (autoload #'mg-send-region-to-python "lib-langs" nil t)
(evil-define-operator mg-send-region-to-python (beg end)
    (python-shell-send-region beg end t))

(defvar mg-ess-httpgd-xwidget-buffer-name "*xwidget webkit: R Plot *"
    "the xwidget buffer name when it is displaying the httpgd
session.")

(defvar mg-python-local-html-xwidget-buffer-name "*xwidget webkit:  *"
    "the xwidget buffer name when it is displaying the local html file
session.")

;;;###autoload
(defun mg-ess-toggle-view-httpgd ()
    "Display the httpgd buffer if not displayed. If the buffer is
displayed, close the window. If no httpgd buffer exists, ask to create
it."
    (interactive)
    (if-let* ((httpgd-buf (get-buffer mg-ess-httpgd-xwidget-buffer-name)))
            (if-let* ((httpgd-win (get-buffer-window httpgd-win)))
                    (delete-window httpgd-win)
                (display-buffer httpgd-buf))
        (call-interactively #'xwidget-webkit-browse-url))
    )

;;;###autoload
(defun mg-python-toggle-view-local-html ()
    "Display the local html buffer if not displayed. If local html
buffer is displayed, close the window. If no local html buffer exists,
ask to create it."
    (interactive)
    (if-let* ((local-html-buf (get-buffer mg-python-local-html-xwidget-buffer-name)))
            (if-let* ((local-html-win (get-buffer-window local-html-buf)))
                    (delete-window local-html-win)
                (display-buffer local-html-buf))
        (call-interactively #'mg-open-html-with-xwidget))
    )

;;;###autoload
(defun mg-run-python (dedicated)
    "Run python in project root that is dedicated to current buffer.
With an prefix \\[universal-argument], make this python session global
(not dedicated to any buffer)."
    (interactive "P")
    (let ((default-directory (mg-project-root-or-default-dir)))
        (run-python nil (not dedicated) 4)))

;;;###autoload
(defun mg-markdown-run-repl ()
    "Run the REPL depending on the context (i.e. the language of the
code block)"
    (interactive)
    ;; `markd-code-block-lang' will move point to the begin of the
    ;; code block, so we `save-excursion'
    (pcase (save-excursion (markdown-code-block-lang))
        ("r" (call-interactively #'termint-radian-start))
        ("R" (call-interactively #'termint-radian-start))
        ("python" (call-interactively #'termint-ipython-start))
        (x "No associated REPL found!")))

;;;###autoload
(defun mg-markdown-hide-window()
    "Close the REPL window denpending on the context (i.e. the
language of the code block)."
    (interactive)
    (pcase (save-excursion (markdown-code-block-lang))
        ("r" (call-interactively #'termint-radian-hide-window))
        ("R" (call-interactively #'#'termint-radian-hide-window))
        ("python" (call-interactively #'termint-ipython-hide-window))
        (x "No associated REPL found!")))

;;;###autoload (autoload #'mg-markdown-send-region "lib-langs" nil t)
(evil-define-operator mg-markdown-send-region (beg end session)
    "Send region to the REPL depending on the context (i.e. the
language of the code block)"
    (interactive "<r>P")
    (pcase (save-excursion (markdown-code-block-lang))
        ("r" (termint-radian-send-region-operator beg end session))
        ("R" (termint-radian-send-region-operator beg end session))
        ("python" (termint-ipython-send-region-operator beg end session))
        (x "No associated REPL found!")))

;;;###autoload (autoload #'mg-markdown-source-region "lib-langs" nil t)
(evil-define-operator mg-markdown-source-region (beg end session)
    "Source region to the REPL depending on the context (i.e. the
language of the code block)"
    (interactive "<r>P")
    (pcase (save-excursion (markdown-code-block-lang))
        ("r" (termint-radian-source-region-operator beg end session))
        ("R" (termint-radian-source-region-operator beg end session))
        ("python" (termint-ipython-source-region-operator beg end session))
        (x "No associated REPL found!")))


(defvar mg-conda-activate-hook nil
    "Hook run after activating a conda environment.
The hook functions are called with one argument: the path to the
activated environment.")

(defvar mg-conda-deactivate-hook nil
    "Hook run after deactivating a conda environment.")

;;;###autoload
(defun mg-conda-activate (&optional path)
    "Activate a conda environment."
    (interactive)
    (mg-conda-deactivate)

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
                (run-hook-with-args 'mg-conda-activate-hook conda-current-env)
                (message "Activating conda environment: %s" path))
        (message "conda not found")))

;;;###autoload
(defun mg-conda-deactivate ()
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
                (run-hooks 'mg-conda-deactivate-hook)
                (message "Conda environment deactivated."))
        (message "conda not found")))



(defvar mg-python-venv-activate-hook nil
    "Hook run after activating a python virtual environment.
The hook functions are called with one argument: the path to the activated environment.")

(defvar mg-python-venv-deactivate-hook nil
    "Hook run after deactivating a python virtual environment.")

;;;###autoload
(defun mg-python-venv-activate (&optional path)
    "Activate a python virtual environment."
    (interactive "Dselect a python venv: ")

    (mg-python-venv-deactivate)

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
        (run-hook-with-args 'mg-python-venv-activate-hook pyvenv-current-env)
        (message "Activating python venv: %s" path)))

;;;###autoload
(defun mg-python-venv-deactivate ()
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
            (run-hooks 'mg-python-venv-deactivate-hook))))


;;;###autoload
(defun mg-poetry-venv-activate (path)
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
    (mg-python-venv-activate (replace-regexp-in-string " (Activated)$" "" path)))

;;;###autoload
(defun mg-poetry-venv-deactivate ()
    "Deactivate the current poetry virtual environment."
    (interactive)
    (mg-python-venv-deactivate))

(defmacro mg--setf-nested-plist (place val &rest attributes)
    "Set a PLIST's nested ATTRIBUTES to VAL.
Example usage: (mg-setf-nested-plist a-plist \"hello\" :level-1 :level-2)."
    (if (null attributes)
            (error "mg-setf-nested-plist requires at least one attribute key"))
    (let ((access-form place))
        (dolist (attr attributes)
            (setq access-form `(plist-get ,access-form ',attr)))
        `(setf ,access-form ,val)))

(advice-add #'jsonrpc-notify :around
            (defun mg-json (old-fun &rest args)
                (setq tompson args)
                (apply old-fun args)))

(defun mg-eglot-update-python-path (path)
    "Updates the Python path used by the Eglot server."
    (when-let* ((current-server (eglot-current-server))
                (server-info (eglot--server-info current-server))
                (is-based-pyright (equal (plist-get server-info :name)
                                         "basedpyright"))
                (config (copy-tree eglot-workspace-configuration)))
        ;; HACK: Eglot uses dir-local variables for
        ;; `eglot-workspace-configuration'.  To programmatically apply
        ;; a specific configuration, this function temporarily advises
        ;; `eglot--workspace-configuration-plist` to return a custom
        ;; settings plist.
        (mg--setf-nested-plist config
                               (concat path "/bin/python3")
                               :python
                               :pythonPath)
        (defalias #'mg--eglot-set-workspace-configuration
            (lambda (&rest _)
                config))
        (advice-add #'eglot--workspace-configuration-plist :around #'mg--eglot-set-workspace-configuration)
        (eglot-signal-didChangeConfiguration current-server)
        ;; `eglot--workspace-configuration-plist' may be invoked
        ;; multiple times by Eglot when settings change. The advice is
        ;; removed via an idle timer to ensure all such invocations
        ;; use the updated configuration before cleanup.
        (run-with-idle-timer
         1 nil
         #'advice-remove #'eglot--workspace-configuration-plist #'mg--eglot-set-workspace-configuration)))


;;;###autoload (autoload #'yapf-format-buffer "lib-langs" nil t)
(reformatter-define yapf-format :program "yapf")

;;;###autoload (autoload #'black-format-buffer "lib-langs" nil t)
(reformatter-define black-format
    :program "black"
    :args '("--quiet" "-"))

;;;###autoload (autoload #'sql-formatter-format-buffer "lib-langs" nil t)
(reformatter-define sql-formatter-format
    :program "sql-formatter"
    :args (when-let* ((config-file (file-name-concat
                                    (getenv "HOME")
                                    ".config"
                                    "sql_formatter"
                                    "sql_formatter.json"))
                      (config-file-exists (file-exists-p config-file)))
              `("--config" ,config-file)))

(defun mg--edit-src-treesit-get-string-range ()
    (let ((node (treesit-node-at (point)))
          (query "((string_content) @str)"))
        (treesit-query-range node query)))

(defvar mg-edit-src-hook-guess-mode-functions '(mg--edit-src-detect-sql)
    "Hooks to detect the mode of the temporary buffer for
edit-src. These functions take one argument which is the content
of the temp buffer.")

(defun mg--edit-src-detect-sql (content)
    (cond
     ((string-match "\\`[ \t\n]*--[ \t]*[sS][qQ][lL]" content) 'sql-mode)
     ((string-match "\\`[ \t\n]*/\\*.*[sS][qQ][lL].*\\*/" content) 'sql-mode)
     (t nil)))

;;;###autoload
(defun mg-edit-src ()
    "Edit the embedded code within a separate buffer."
    (interactive)
    (require 'edit-indirect)
    (when-let* ((range (mg--edit-src-treesit-get-string-range))
                (beg (caar range))
                (end (cdar range))
                (content (buffer-substring-no-properties beg end))
                (edit-indirect-guess-mode-function
                 (lambda (&rest _)
                     (funcall
                      (or (run-hook-with-args-until-success 'mg-edit-src-hook-guess-mode-functions content)
                          'normal-mode)))))
        (edit-indirect-region beg end t)))

(provide 'lib-langs)
;;; lib-langs.el ends here
