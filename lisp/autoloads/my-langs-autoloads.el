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
        ("python" (call-interactively #'my/run-python))
        (x "No associated REPL found!")))

;;;###autoload (autoload #'my/markdown-send-region "my-langs-autoloads" nil t)
(evil-define-operator my/markdown-send-region (beg end)
    "Send region to the REPL depending on the context (i.e. the
language of the code block)"
    (pcase (markdown-code-block-lang)
        ("r" (my/send-region-to-ess beg end))
        ("R" (my/send-region-to-ess beg end))
        ("python" (my/send-region-to-python beg end))
        (x "No associated REPL found!")))

(defvar my$conda-current-env nil
    "The path to the current conda environment.")

;;;###autoload
(defun my~conda-activate (&optional path)
    "This command activates a conda environment, assuming that the
base environment is already activated.  If the environment variable
CONDA_PREFIX is not present, this command will not perform any
action."
    (interactive
     `(,(read-directory-name
         "select a conda env"
         (file-name-concat
          (or (getenv "CONDA_PREFIX_1")
              (getenv "CONDA_PREFIX"))
          "envs/"))))

    (my~conda-deactivate)

    ;; if the path contains trailing "bin" or "bin/", remove it
    (let (conda-current-env)

        ;; get the absolute path of path
        (setq path (expand-file-name path)
              path (replace-regexp-in-string "/bin/?$" "" path)
              ;; if the path contains trailing "/", remove it
              path (replace-regexp-in-string "/$" "" path)
              conda-current-env path
              ;; append the path with "/bin"
              path (concat path "/bin")
              my$conda-current-env path)

        (setenv "PATH" (concat path ":" (getenv "PATH")))
        (setenv "CONDA_PREFIX_1" (getenv "CONDA_PREFIX"))
        (setenv "CONDA_PREFIX" conda-current-env)

        (message "Activating conda environment: %s" path)))

;;;###autoload
(defun my~conda-deactivate ()
    "This command deactivates the current conda environment, except
for the base environment."
    (interactive)
    (when my$conda-current-env
        ;; split the PATH
        (let ((paths (split-string (getenv "PATH") ":")))
            ;; remove the path to the current conda environment
            (setq paths (delete my$conda-current-env paths)
                  my$conda-current-env nil)
            ;; set the PATH
            (setenv "PATH" (string-join paths ":"))
            (setenv "CONDA_PREFIX" (getenv "CONDA_PREFIX_1"))
            (setenv "CONDA_PREFIX_1" nil))))

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

(provide 'my-langs-autoloads)
;;; my-init-langs.el ends here
