;;; my-apps-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/open-html-with-xwidget (url new-session)
    "open local file (html) using xwidget,
prefix with C-u to open the url with a new xwidget session"
    (interactive "G\nP")
    (setq url (concat "file://"
                      (expand-file-name url)))
    (xwidget-webkit-browse-url url new-session))

(defun my/google-search-wrapper (browser-func)
    "search goole keywords with browser specified by `browser-func'"
    (lambda (keyword &optional new-session)
        (interactive
         ;; copied and adapted from `browse-url-interactive-arg'
         (list
          (read-string
           "google search: "
           ;; short circuiting
           ;; or/and returns the value itself (not t and nil)
           (or (and transient-mark-mode mark-active
                    (replace-regexp-in-string
                     "[\t\r\f\n ]+" " "
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))))
               (current-word)))))
        ;; referenced from
        ;; URL `https://github.com/manateelazycat/blink-search/blob/master/backend/search_google_suggest.py'
        ;; space is illegal in an url
        (let* ((keyword (replace-regexp-in-string " " "%20" keyword))
               ;; url is expected to not have Chinese. Should properly encode it.
               (url (url-encode-url (concat "http://www.google.com/search?q=" keyword))))
            (funcall browser-func url new-session))))

;;;###autoload (autoload #'my/google-search-eww "my-apps-autoloads" nil t)
(defalias #'my/google-search-eww (my/google-search-wrapper #'eww)
    "Search google keywords by `eww'.
If region is active, use the region as keyword of initial input, otherwise use `current-word'.")

;;;###autoload (autoload #'my/google-search-xwidget "my-apps-autoloads" nil t)
(defalias #'my/google-search-xwidget
    (my/google-search-wrapper #'xwidget-webkit-browse-url)
    "Search google keywords by `xwidget-webkit-browse-url'.
If region is active, use the region as keyword of initial input, otherwise use `current-word'.")

(defun my/refresh-xwidget-after-eval-python (&rest _)
    (run-with-idle-timer 3 nil #'xwidget-webkit-reload))

;;;###autoload
(define-minor-mode my/refresh-xwidget-after-eval-python-mode
    "After evaluating a python command, typically like
`python-shell-send-defun', `python-shell-send-region', refreshing the
xwidget browser. This is useful for interactive usage with web stuffs
like plotly."
    :global t

    (if my/refresh-xwidget-after-eval-python-mode
            (progn
                (advice-add #'python-shell-send-statement
                            :after
                            #'my/refresh-xwidget-after-eval-python)
                (advice-add #'python-shell-send-region
                            :after
                            #'my/refresh-xwidget-after-eval-python))
        (progn
            (advice-remove #'python-shell-send-statement
                           #'my/refresh-xwidget-after-eval-python)
            (advice-remove #'python-shell-send-region
                           #'my/refresh-xwidget-after-eval-python)))
    )

(defun my/switch-to-buffer-obey-display-actions (old-fun &rest args)
    (let ((switch-to-buffer-obey-display-actions t))
        (apply old-fun args)))

(defvar my/xwidget-side-window-display
    `("\\*xwidget"
      (display-buffer-in-side-window display-buffer-reuse-window))
    "the display action used for xwidget when use it as a side window.")

;;;###autoload
(define-minor-mode my/xwidget-side-window-mode
    "`xwidget-webkit-browse-url' doesn't respect
`display-buffer-alist'.  This minor mode advises
`xwidget-webkit-browse-url' to make it respect such. This is helpful
for interactive plotting usage with python/R where you typically want
xwdiget to display plots at the side window."
    :global t

    (unless (require 'xwidget nil t)
        (error "this mode requires xwidget!"))

    (if my/xwidget-side-window-mode
            (progn
                (add-to-list 'display-buffer-alist my/xwidget-side-window-display)
                (advice-add #'xwidget-webkit-new-session :around #'my/switch-to-buffer-obey-display-actions)
                (advice-add #'xwidget-webkit-goto-url :around #'my/switch-to-buffer-obey-display-actions))
        (progn
            (setq display-buffer-alist (remove my/xwidget-side-window-display display-buffer-alist))
            (advice-remove #'xwidget-webkit-new-session #'my/switch-to-buffer-obey-display-actions)
            (advice-remove #'xwidget-webkit-goto-url #'my/switch-to-buffer-obey-display-actions)))
    )

(provide 'my-apps-autoloads)
;;; my-apps-autoloads.el ends here
