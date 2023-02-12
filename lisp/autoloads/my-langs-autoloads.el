;;; my-langs-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/ess-set-company-backend ()
    (setq-local company-backends
                '((company-capf company-files
                                company-R-library company-R-args company-R-objects
                                :separate company-dabbrev
                                :with company-yasnippet))))
;;;###autoload
(defun my/ess-set-tab-width-4 ()
    (setq-local tab-width 4))

;;;###autoload (autoload #'my/send-region-to-ess "my-langs-autoloads" nil t)
(evil-define-operator my/send-region-to-ess (beg end)
    "This operator sends the region (either motion or text objects) to ess REPL"
    ;; t means don't echo the region in the ess REPL buffer
    (ess-eval-region beg end t))

;;;###autoload (autoload #'my/send-region-to-python "my-langs-autoloads" nil t)
(evil-define-operator my/send-region-to-python (beg end)
    (python-shell-send-region beg end t))

;;;###autoload
(defun my/poly-mode-disable-flymake (old-buf new-buf)
    "poly-mode are duplicated buffers with exactly the
same buffer content, when you are on `prog-mode' then your code linter
will be perplexed by those non-code content. So disable flymake in
poly-mode."
    (with-current-buffer new-buf
        (when flymake-mode
            (flymake-mode -1))))

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
(defun my/run-python ()
    "Run python in project root that is dedicated to current buffer."
    (interactive)
    (let ((default-directory (or (consult--project-root)
                                 default-directory)))
        (run-python nil t 4)))

(provide 'my-langs-autoloads)
;;; my-init-langs.el ends here
