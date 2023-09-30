;;; my-langs-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload (autoload #'my/send-region-to-ess "my-langs-autoloads" nil t)
(evil-define-operator my/send-region-to-ess (beg end)
    "This operator sends the region (either motion or text objects) to ess REPL"
    ;; t means don't echo the region in the ess REPL buffer
    (ess-eval-region beg end t))

;;;###autoload (autoload #'my/send-region-to-python "my-langs-autoloads" nil t)
(evil-define-operator my/send-region-to-python (beg end)
    (python-shell-send-region beg end t))

(defvar my/ess-httpgd-xwidget-buffer-name "*xwidget webkit: R Plot *"
    "the xwidget buffer name when it is displaying the httpgd
session.")

(defvar my/python-local-html-xwidget-buffer-name "*xwidget webkit:  *"
    "the xwidget buffer name when it is displaying the local html file
session.")

;;;###autoload
(defun my/ess-toggle-view-httpgd ()
    "Display the httpgd buffer if not displayed. If the buffer is
displayed, close the window. If no httpgd buffer exists, ask to create
it."
    (interactive)
    (if-let ((httpgd-buf (get-buffer my/ess-httpgd-xwidget-buffer-name)))
            (if-let ((httpgd-win (get-buffer-window httpgd-win)))
                    (delete-window httpgd-win)
                (display-buffer httpgd-buf))
        (call-interactively #'xwidget-webkit-browse-url))
    )

;;;###autoload
(defun my/python-toggle-view-local-html ()
    "Display the local html buffer if not displayed. If local html
buffer is displayed, close the window. If no local html buffer exists,
ask to create it."
    (interactive)
    (if-let ((local-html-buf (get-buffer my/python-local-html-xwidget-buffer-name)))
            (if-let ((local-html-win (get-buffer-window local-html-buf)))
                    (delete-window local-html-win)
                (display-buffer local-html-buf))
        (call-interactively #'my/open-html-with-xwidget))
    )

;;;###autoload
(defun my/run-python (dedicated)
    "Run python in project root that is dedicated to current buffer.
With an prefix \\[universal-argument], make this python session global
(not dedicated to any buffer)."
    (interactive "P")
    (let ((default-directory (my/project-root-or-default-dir)))
        (run-python nil (not dedicated) 4)))

;;;###autoload
(defun my/markdown-run-repl ()
    "Run the REPL depending on the context (i.e. the language of the
code block)"
    (interactive)
    (pcase (markdown-code-block-lang)
        ("r" (call-interactively #'run-ess-r))
        ("R" (call-interactively #'run-ess-r))
        ("python" (call-interactively #'my~ipython-start))
        (x "No associated REPL found!")))

;;;###autoload (autoload #'my/markdown-send-region "my-langs-autoloads" nil t)
(evil-define-operator my/markdown-send-region (beg end session)
    "Send region to the REPL depending on the context (i.e. the
language of the code block)"
    (interactive "<r>P")
    (pcase (markdown-code-block-lang)
        ("r" (my/send-region-to-ess beg end))
        ("R" (my/send-region-to-ess beg end))
        ("python" (my~ipython-send-region-operator beg end session))
        (x "No associated REPL found!")))

(defvar my$conda-current-env nil
    "The path to the current conda environment.")

;;;###autoload
(defun my~conda-activate (&optional path)
    "Activate a conda environment."
    (interactive)
    (my~conda-deactivate)

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
                      path (concat path "/bin")
                      my$conda-current-env conda-current-env)

                (setenv "PATH" (concat path ":" (getenv "PATH")))
                (setenv "CONDA_PREFIX" conda-current-env)
                (setenv "CONDA_DEFAULT_ENV" (file-name-nondirectory conda-current-env))
                (setenv "CONDA_PROMPT_MODIFIER" (concat "(" (file-name-nondirectory conda-current-env) ") "))
                (setenv "CONDA_SHLVL" "1")
                (message "Activating conda environment: %s" path))
        (message "conda not found")))

;;;###autoload
(defun my~conda-deactivate ()
    "Deactivate all the conda environments, including the base environment."
    (interactive)
    (if (executable-find "conda")
            (let ((paths (split-string (getenv "PATH") ":"))
                  (conda-info (json-parse-string (shell-command-to-string "conda info --json")
                                                 :object-type 'plist
                                                 :array-type 'list)))
                (setq my$conda-current-env (or my$conda-current-env (plist-get conda-info :root_prefix))
                      paths (delete (concat my$conda-current-env "/bin") paths)
                      my$conda-current-env nil)
                (setenv "PATH" (string-join paths ":"))
                (setenv "CONDA_PREFIX" nil)
                (setenv "CONDA_DEFAULT_ENV" nil)
                (setenv "CONDA_SHLVL" "0")
                (setenv "CONDA_PROMPT_MODIFIER" nil)
                (message "Conda environment deactivated."))
        (message "conda not found")))

(defvar my$python-venv-current-env nil
    "The path to the current python venv environment.")

;;;###autoload
(defun my~python-venv-activate (&optional path)
    "This command activates a python virtual environment."
    (interactive "Dselect a python venv: ")

    (my~python-venv-deactivate)

    ;; if the path contains trailing "bin" or "bin/", remove it
    (let (pyvenv-current-env)

        (setq path (expand-file-name path)
              path (replace-regexp-in-string "/bin/?$" "" path)
              ;; if the path contains trailing "/", remove it
              path (replace-regexp-in-string "/$" "" path)
              pyvenv-current-env path
              ;; append the path with "/bin"
              python-shell-virtualenv-root pyvenv-current-env
              path (concat path "/bin")
              my$python-venv-current-env path)

        (setenv "PATH" (concat path ":" (getenv "PATH")))
        (setenv "VIRTUAL_ENV" pyvenv-current-env)

        (message "Activating python venv: %s" path)))

;;;###autoload
(defun my~python-venv-deactivate ()
    "This command deactivates the current python virtual environment."
    (interactive)
    (when my$python-venv-current-env
        ;; split the PATH
        (let ((paths (split-string (getenv "PATH") ":")))
            ;; remove the path to the current python venv environment
            (setq paths (delete my$python-venv-current-env paths)
                  my$python-venv-current-env nil
                  python-shell-virtualenv-root nil)
            ;; set the PATH
            (setenv "PATH" (string-join paths ":"))
            (setenv "VIRTUAL_ENV" nil))))

;;;###autoload (autoload #'yapf-format-buffer "my-langs-autoloads" nil t)
(reformatter-define yapf-format :program "yapf")

;;;###autoload (autoload #'black-format-buffer "my-langs-autoloads" nil t)
(reformatter-define black-format
    :program "black"
    :args '("--quiet" "-"))

;;;###autoload (autoload #'sql-formatter-format-buffer "my-langs-autoloads" nil t)
(reformatter-define sql-formatter-format
    :program "sql-formatter"
    :args (when-let* ((config-file (file-name-concat
                                    (getenv "HOME")
                                    ".config"
                                    "sql_formatter"
                                    "sql_formatter.json"))
                      (config-file-exists (file-exists-p config-file)))
              `("--config" ,config-file)))

(provide 'my-langs-autoloads)
;;; my-init-langs.el ends here
