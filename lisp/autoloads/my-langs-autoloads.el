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

(provide 'my-langs-autoloads)
;;; my-init-langs.el ends here
