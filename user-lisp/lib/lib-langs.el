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

(provide 'lib-langs)
;;; lib-langs.el ends here
