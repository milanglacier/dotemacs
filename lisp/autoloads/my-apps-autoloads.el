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
    (when (xwidget-webkit-last-session)
        (run-with-idle-timer 3 nil #'xwidget-webkit-reload)))

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

(defvar my/xwidget-side-window-display
    '("\\*xwidget"
      (display-buffer-in-side-window display-buffer-reuse-window))
    "the display action used for xwidget when use it as a side window.")

(defvar my/xwidget-force-display-action
    '(display-buffer-same-window)
    "the display action used for `my/xwidget-force-display-mode'")

(defun my/switch-to-buffer-obey-display-actions (old-fun &rest args)
    (let ((switch-to-buffer-obey-display-actions t))
        (apply old-fun args)))

(defun my/xwidget-force-display (&rest args)
    (if-let ((session (xwidget-webkit-current-session)))
            (display-buffer (xwidget-buffer session)
                            my/xwidget-force-display-action)))

;;;###autoload
(define-minor-mode my/xwidget-side-window-mode
    "`xwidget-webkit-browse-url' doesn't respect
`display-buffer-alist'.  This minor mode advises
`xwidget-webkit-browse-url' to make it respect such. This is helpful
for interactive plotting usage with python/R where you typically want
xwdiget to display plots at the side window."
    :global t

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

;;;###autoload
(define-minor-mode my/xwidget-force-display-mode
    "`xwidget-webkit-browse-url' won't display its buffer in current
frame when the xwidget session exists and no window is displaying that
session.  This minor mode advises `xwidget-webkit-browse-url' to
ensure such behavior. This is helpful for viewing web contents with
`mu4e', `notmuch', and `elfeed'"
    :global t

    (if my/xwidget-force-display-mode
            (progn
                (advice-add #'xwidget-webkit-goto-url :after #'my/xwidget-force-display))
        (progn
            (advice-remove #'xwidget-webkit-goto-url #'my/xwidget-force-display)))
    )

;;;###autoload
(defun my:elfeed-delete-window-after-kill-buffer (&rest args)
    (delete-window (selected-window)))

;;;###autoload
(defun my:elfeed-open-entry-via-xwidget (&optional new-session)
    "if point is under a url, then open this url via `xwidget',
otherwise open the current visited elfeed entry via `xwidget'.  If
with a prefix \\[universal-argument] create a new `xwidget' session
otherwise use the existed one"
    (interactive "P")
    (if-let ((link-at-point (get-text-property (point) 'shr-url)))
            (xwidget-webkit-browse-url link-at-point new-session)
        (xwidget-webkit-browse-url
         (elfeed-entry-link elfeed-show-entry)
         new-session)))

;;;###autoload
(defun my:elfeed-open-entry-via-eww (&optional new-session)
    "if point is under a url, then open this url via `eww',
otherwise open the current visited elfeed entry via `eww'.  If
with a prefix \\[universal-argument] create a new `eww' session
otherwise use the existed one"
    (interactive "P")
    (if-let ((link-at-point (get-text-property (point) 'shr-url)))
            (eww link-at-point new-session)
        (eww
         (elfeed-entry-link elfeed-show-entry)
         new-session)))

;;;###autoload
(defun my:pdf-midnight-mode-maybe ()
    (when (eq (frame-parameter nil 'background-mode) 'dark)
        (pdf-view-midnight-minor-mode)
        (pdf-view-dark-minor-mode)))

(defmacro my%create-vterm-repl-schema (repl-name repl-cmd &rest args)
    "create a REPL schema.

The REPL session will be created via vterm. The schema includes three
functions, the function to start the repl, the function to send the
region and the corresponding operator.

REPL-NAME is a string, REPL-CMD is a form evaluated to a string. ARGS
is a plist, the following properties are supported:

:bracketed-paste-p whether send the string with bracketed paste mode,
the default value is nil.  You can change the behavior at run time by
setting the generated variable
`my*REPL-NAME-use-bracketed-paste-mode'.

:start-pattern the first string to send to the REPl before sending the
region. The default is ''.  You can change the behavior at run time by
setting the generated variable `my*REPL-NAME-start-pattern'.

:end-pattern the last string to send to the REPL after sending the
region. The default is '\\r'.  You can change the behavior at run time
by setting the generated variable `my*REPL-NAME-end-pattern'.

:str-process-func the function to process the string before sending it
to the REPL.  The default is `identity'. You can change the behavior
at run time by setting the generated variable
`my*REPL-NAME-str-process-func'."

    (let ((start-func-name (intern (concat "my~" repl-name "-start")))
          (send-region-func-name (intern (concat "my~" repl-name "-send-region")))
          (send-region-operator-name (intern (concat "my~" repl-name "-send-region-operator")))
          (bracketed-paste-p (plist-get args :bracketed-paste-p))
          (start-pattern (or (plist-get args :start-pattern) ""))
          (end-pattern (or (plist-get args :end-pattern) "\r"))
          (str-process-func (or (plist-get args :str-process-func) ''identity))
          (str-process-func-name (intern (concat "my*" repl-name "-str-process-func")))
          (bracketed-paste-p-name (intern (concat "my*" repl-name "-use-bracketed-paste-mode")))
          (start-pattern-name (intern (concat "my*" repl-name "-start-pattern")))
          (end-pattern-name (intern (concat "my*" repl-name "-end-pattern"))))

        `(progn

             (defvar ,str-process-func-name ,str-process-func
                 ,(format "The function to process the string before sending it to the %s REPL." repl-name))

             (defvar ,bracketed-paste-p-name ,bracketed-paste-p
                 ,(format "Whether use bracketed paste mode for sending string to the %s REPL." repl-name))

             (defvar ,start-pattern-name ,start-pattern
                 ,(format "The first string to send to the %s REPL before sending the text." repl-name))

             (defvar ,end-pattern-name ,end-pattern
                 ,(format "The last string to send to the %s REPL after sending the text." repl-name))

             (defun ,start-func-name (&optional arg)
                 ,(format
                   "Create a %s REPL buffer.
Start a new %s session or switch to an already active session. Return
the buffer selected (or created). With a numeric prefix arg,create or
switch to the session with that number as a suffix."
                   repl-name repl-name)
                 (interactive "P")
                 (require 'vterm)
                 (let ((vterm-buffer-name (format "*%s*" ,repl-name))
                       (vterm-shell ,repl-cmd)
                       (repl-buffer)
                       (repl-buffer-exist-p
                        (get-buffer
                         (if arg (format "*%s*<%d>" ,repl-name arg)
                             (format "*%s*" ,repl-name)))))
                     (setq repl-buffer (vterm arg))))

             (defun ,send-region-func-name (beg end &optional session)
                 ,(format
                   "Send the region delimited by BEG and END to inferior %s.
With numeric prefix argument, send region to the process associated
with that number." repl-name)
                 (let ((repl-buffer-name
                        (if session
                                (format "*%s*<%d>" ,repl-name session)
                            (format "*%s*" ,repl-name)))
                       (str (buffer-substring-no-properties beg end)))
                     (with-current-buffer repl-buffer-name
                         (vterm-send-string ,start-pattern-name)
                         (vterm-send-string (funcall ,str-process-func-name str) ,bracketed-paste-p-name)
                         (vterm-send-string ,end-pattern-name))))

             (evil-define-operator ,send-region-operator-name (beg end session)
                 ,(format
                   "A evil operator wrapper around `%s'. With a numeric
prefix argument, send the region to the %s process associated with
that number" send-region-func-name repl-name)
                 (interactive "<r>P")
                 (,send-region-func-name beg end session))

             )))

;;;###autoload (autoload #'my~aichat-start "my-apps-autoloads" nil t)
(my%create-vterm-repl-schema "aichat" "aichat -s" :bracketed-paste-p t)

;;;###autoload (autoload #'my~ipython-start "my-apps-autoloads" nil t)
(my%create-vterm-repl-schema "ipython" "ipython" :bracketed-paste-p t)

(provide 'my-apps-autoloads)
;;; my-apps-autoloads.el ends here
