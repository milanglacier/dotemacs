;;; lib-apps.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mg-open-html-with-xwidget (url new-session)
    "open local file (html) using xwidget,
prefix with C-u to open the url with a new xwidget session"
    (interactive "G\nP")
    (setq url (concat "file://"
                      (expand-file-name url)))
    (xwidget-webkit-browse-url url new-session))

(defun mg-google-search-wrapper (browser-func)
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

;;;###autoload (autoload #'mg-google-search-eww "lib-apps" nil t)
(defalias #'mg-google-search-eww (mg-google-search-wrapper #'eww)
    "Search google keywords by `eww'.
If region is active, use the region as keyword of initial input, otherwise use `current-word'.")

;;;###autoload (autoload #'mg-google-search-xwidget "lib-apps" nil t)
(defalias #'mg-google-search-xwidget
    (mg-google-search-wrapper #'xwidget-webkit-browse-url)
    "Search google keywords by `xwidget-webkit-browse-url'.
If region is active, use the region as keyword of initial input, otherwise use `current-word'.")

(defun mg-refresh-xwidget-after-eval-python (&rest _)
    (when (xwidget-webkit-last-session)
        (run-with-idle-timer 3 nil #'xwidget-webkit-reload)))

;;;###autoload
(define-minor-mode mg-refresh-xwidget-after-eval-python-mode
    "After evaluating a python command, typically like
`python-shell-send-defun', `python-shell-send-region', refreshing the
xwidget browser. This is useful for interactive usage with web stuffs
like plotly."
    :global t

    (if mg-refresh-xwidget-after-eval-python-mode
            (progn
                (advice-add #'python-shell-send-statement
                            :after
                            #'mg-refresh-xwidget-after-eval-python)
                (advice-add #'python-shell-send-region
                            :after
                            #'mg-refresh-xwidget-after-eval-python))
        (progn
            (advice-remove #'python-shell-send-statement
                           #'mg-refresh-xwidget-after-eval-python)
            (advice-remove #'python-shell-send-region
                           #'mg-refresh-xwidget-after-eval-python)))
    )

(defvar mg-xwidget-side-window-display
    '("\\*xwidget"
      (display-buffer-in-side-window display-buffer-reuse-window))
    "the display action used for xwidget when use it as a side window.")

(defvar mg-xwidget-force-display-action
    '(display-buffer-same-window)
    "the display action used for `mg-xwidget-force-display-mode'")

(defun mg-switch-to-buffer-obey-display-actions (old-fun &rest args)
    (let ((switch-to-buffer-obey-display-actions t))
        (apply old-fun args)))

(defun mg-xwidget-force-display (&rest args)
    (if-let* ((session (xwidget-webkit-current-session)))
            (display-buffer (xwidget-buffer session)
                            mg-xwidget-force-display-action)))

;;;###autoload
(define-minor-mode mg-xwidget-side-window-mode
    "`xwidget-webkit-browse-url' doesn't respect
`display-buffer-alist'.  This minor mode advises
`xwidget-webkit-browse-url' to make it respect such. This is helpful
for interactive plotting usage with python/R where you typically want
xwdiget to display plots at the side window."
    :global t

    (if mg-xwidget-side-window-mode
            (progn
                (add-to-list 'display-buffer-alist mg-xwidget-side-window-display)
                (advice-add #'xwidget-webkit-new-session :around #'mg-switch-to-buffer-obey-display-actions)
                (advice-add #'xwidget-webkit-goto-url :around #'mg-switch-to-buffer-obey-display-actions))
        (progn
            (setq display-buffer-alist (remove mg-xwidget-side-window-display display-buffer-alist))
            (advice-remove #'xwidget-webkit-new-session #'mg-switch-to-buffer-obey-display-actions)
            (advice-remove #'xwidget-webkit-goto-url #'mg-switch-to-buffer-obey-display-actions)))
    )

;;;###autoload
(define-minor-mode mg-xwidget-force-display-mode
    "`xwidget-webkit-browse-url' won't display its buffer in current
frame when the xwidget session exists and no window is displaying that
session.  This minor mode advises `xwidget-webkit-browse-url' to
ensure such behavior. This is helpful for viewing web contents with
`mu4e', `notmuch', and `elfeed'"
    :global t

    (if mg-xwidget-force-display-mode
            (progn
                (advice-add #'xwidget-webkit-goto-url :after #'mg-xwidget-force-display))
        (progn
            (advice-remove #'xwidget-webkit-goto-url #'mg-xwidget-force-display)))
    )

;;;###autoload
(defun mg--elfeed-delete-window-after-kill-buffer (&rest args)
    (delete-window (selected-window)))

;;;###autoload
(defun mg--elfeed-open-entry-via-xwidget (&optional new-session)
    "if point is under a url, then open this url via `xwidget',
otherwise open the current visited elfeed entry via `xwidget'.  If
with a prefix \\[universal-argument] create a new `xwidget' session
otherwise use the existed one"
    (interactive "P")
    (if-let* ((link-at-point (get-text-property (point) 'shr-url)))
            (xwidget-webkit-browse-url link-at-point new-session)
        (xwidget-webkit-browse-url
         (elfeed-entry-link elfeed-show-entry)
         new-session)))

;;;###autoload
(defun mg--elfeed-open-entry-via-eww (&optional new-session)
    "if point is under a url, then open this url via `eww',
otherwise open the current visited elfeed entry via `eww'.  If
with a prefix \\[universal-argument] create a new `eww' session
otherwise use the existed one"
    (interactive "P")
    (if-let* ((link-at-point (get-text-property (point) 'shr-url)))
            (eww link-at-point new-session)
        (eww
         (elfeed-entry-link elfeed-show-entry)
         new-session)))

;;;###autoload
(defun mg--pdf-midnight-mode-maybe ()
    (when (eq (frame-parameter nil 'background-mode) 'dark)
        (pdf-view-midnight-minor-mode)
        (pdf-view-dark-minor-mode)))

(provide 'lib-apps)
;;; lib-apps.el ends here
