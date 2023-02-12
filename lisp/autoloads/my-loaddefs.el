;;; my-loaddefs.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "my-apps-autoloads" "my-apps-autoloads.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-apps-autoloads.el

(autoload 'my/open-html-with-xwidget "my-apps-autoloads" "\
open local file (html) using xwidget,
prefix with C-u to open the url with a new xwidget session

\(fn URL NEW-SESSION)" t nil)
 (autoload #'my/google-search-eww "my-apps-autoloads" nil t)
 (autoload #'my/google-search-eww "my-apps-autoloads" nil t)

(defvar my/refresh-xwidget-after-eval-python-mode nil "\
Non-nil if My/Refresh-Xwidget-After-Eval-Python mode is enabled.
See the `my/refresh-xwidget-after-eval-python-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `my/refresh-xwidget-after-eval-python-mode'.")

(custom-autoload 'my/refresh-xwidget-after-eval-python-mode "my-apps-autoloads" nil)

(autoload 'my/refresh-xwidget-after-eval-python-mode "my-apps-autoloads" "\
After evaluating a python command, typically like
`python-shell-send-defun', `python-shell-send-region', refreshing the
xwidget browser. This is useful for interactive usage with web stuffs
like plotly.

This is a minor mode.  If called interactively, toggle the
`My/Refresh-Xwidget-After-Eval-Python mode' mode.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value
\\='my/refresh-xwidget-after-eval-python-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(defvar my/xwidget-side-window-mode nil "\
Non-nil if My/Xwidget-Side-Window mode is enabled.
See the `my/xwidget-side-window-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `my/xwidget-side-window-mode'.")

(custom-autoload 'my/xwidget-side-window-mode "my-apps-autoloads" nil)

(autoload 'my/xwidget-side-window-mode "my-apps-autoloads" "\
`xwidget-webkit-browse-url' doesn't respect
`display-buffer-alist'.  This minor mode advises
`xwidget-webkit-browse-url' to make it respect such. This is helpful
for interactive plotting usage with python/R where you typically want
xwdiget to display plots at the side window.

This is a minor mode.  If called interactively, toggle the
`My/Xwidget-Side-Window mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='my/xwidget-side-window-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "my-apps-autoloads" '("my/"))

;;;***

;;;### (autoloads nil "my-basics-autoloads" "my-basics-autoloads.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-basics-autoloads.el

(autoload 'my/display-truncation-and-wrap-indicator-as-whitespace "my-basics-autoloads" nil nil nil)

;;;***

;;;### (autoloads nil "my-completion-autoloads" "my-completion-autoloads.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-completion-autoloads.el

(autoload 'my/company-abort "my-completion-autoloads" nil nil nil)

(autoload 'my/company-completion-styles "my-completion-autoloads" "\


\(fn CAPF-FN &rest ARGS)" nil nil)

;;;***

;;;### (autoloads nil "my-elisp-autoloads" "my-elisp-autoloads.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-elisp-autoloads.el

(autoload 'my/helpful-display-buffer "my-elisp-autoloads" "\
If a helpful buffer window is already opened, should
use it, don't occupy other window. Make sure it is a side window, such
that when you want to dwim (pressing q and want to close the help
window), this window will be completely removed, i.e. the window won't
be displayed showing other buffer.

\(fn BUF)" nil nil)

(autoload 'my/helpful-lookup-symbl-at-point "my-elisp-autoloads" nil t nil)

(autoload 'my/elisp-loop-up-symbol "my-elisp-autoloads" "\
Look up for the symbol under point, if region is active, use
        the selected region as the symbol

\(fn BEG END)" t nil)

(autoload 'my/elisp-setup "my-elisp-autoloads" nil nil nil)

(register-definition-prefixes "my-elisp-autoloads" '("my/emacs-lisp-outline-level"))

;;;***

;;;### (autoloads nil "my-evil-autoloads" "my-evil-autoloads.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-evil-autoloads.el

(autoload 'my/save-excursion-before-indenting "my-evil-autoloads" "\


\(fn ORIGIN-FN &rest ARGS)" nil nil)
 (autoload #'my/evil-apply-macro-line-by-line "my-evil-autoloads" nil t)

(autoload 'my/define-and-bind-paren-text-object "my-evil-autoloads" "\


\(fn KEY START-REGEX END-REGEX)" nil t)

(autoload 'my/define-and-bind-local-paren-text-object "my-evil-autoloads" "\


\(fn KEY START-REGEX END-REGEX HOOK)" nil t)

;;;***

;;;### (autoloads nil "my-langs-autoloads" "my-langs-autoloads.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-langs-autoloads.el

(autoload 'my/ess-set-company-backend "my-langs-autoloads" nil nil nil)

(autoload 'my/ess-set-tab-width-4 "my-langs-autoloads" nil nil nil)
 (autoload #'my/send-region-to-ess "my-langs-autoloads" nil t)
 (autoload #'my/send-region-to-python "my-langs-autoloads" nil t)

(autoload 'my/poly-mode-disable-flymake "my-langs-autoloads" "\
poly-mode are duplicated buffers with exactly the
same buffer content, when you are on `prog-mode' then your code linter
will be perplexed by those non-code content. So disable flymake in
poly-mode.

\(fn OLD-BUF NEW-BUF)" nil nil)

(autoload 'my/ess-toggle-view-httpgd "my-langs-autoloads" "\
Display the httpgd buffer if not displayed. If the buffer is
displayed, close the window. If no httpgd buffer exists, ask to create
it." t nil)

(autoload 'my/python-toggle-view-local-html "my-langs-autoloads" "\
Display the local html buffer if not displayed. If local html
buffer is displayed, close the window. If no local html buffer exists,
ask to create it." t nil)

(autoload 'my/run-python "my-langs-autoloads" "\
Run python in project root that is dedicated to current buffer." t nil)

(register-definition-prefixes "my-langs-autoloads" '("my/"))

;;;***

;;;### (autoloads nil "my-langtools-autoloads" "my-langtools-autoloads.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-langtools-autoloads.el

(autoload 'my/do-not-use-citre-imenu "my-langtools-autoloads" nil nil nil)

(autoload 'my/do-not-use-citre-xref "my-langtools-autoloads" nil nil nil)

(autoload 'my/do-not-use-citre-capf "my-langtools-autoloads" nil nil nil)

(autoload 'my/toggle-citre-eglot-capf "my-langtools-autoloads" nil nil nil)

(autoload 'my/eglot-do-not-use-imenu "my-langtools-autoloads" nil nil nil)

(autoload 'my/eldoc-buffer-dwim "my-langtools-autoloads" "\
When eldoc buffer window is not opened, display the eldoc
window. Pressing `my/eldoc-buffer-dwim-key' again within a short
period (1s currently as hard coded) will move your focus on the eldoc
window. If the shorter period has gone, calling this command will
close the eldoc window. Currently this dwim hack is only effective in
`eglot-mode-map' as it is hardcoded." t nil)

(autoload 'my/xref-move-in-original-src-macro "my-langtools-autoloads" "\
There can only be one xref buffer. That is, if you find
references of other symbol the previous one will be overwritten. The
official `xref-next-line' `xref-next-group' only allows you to move
the location in the src buffer when your point is in the xref buffer
window. This macro creates funcs that allow you to move current window
to next xref location.

\(fn FUNC)" nil t)

(register-definition-prefixes "my-langtools-autoloads" '("my/eldoc-"))

;;;***

;;;### (autoloads nil "my-minibuffer-autoloads" "my-minibuffer-autoloads.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-minibuffer-autoloads.el

(autoload 'crm-indicator "my-minibuffer-autoloads" "\


\(fn ARGS)" nil nil)

(autoload 'my/completion-in-region "my-minibuffer-autoloads" "\


\(fn &rest ARGS)" nil nil)
 (autoload #'my/evil-delete-in-wgrep "my-minibuffer-autoloads" nil t)

;;;***

;;;### (autoloads nil "my-misc-autoloads" "my-misc-autoloads.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-misc-autoloads.el

(autoload 'my/vterm "my-misc-autoloads" "\
open vterm at project root, if no root is found, open at the default-directory" t nil)

(autoload 'my/vterm-setup "my-misc-autoloads" nil nil nil)

(autoload 'my/ibuffer-vc-setup "my-misc-autoloads" nil nil nil)

;;;***

;;;### (autoloads nil "my-org-autoloads" "my-org-autoloads.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from my-org-autoloads.el

(autoload 'my/load-org-extensions-idly "my-org-autoloads" "\
Some important variables from other org extensions are not autoloaded.
You may feel annoying if you want to use them but find a void variable.
\(e.g. you want to call `org-open-at-point' on a timestamp)" nil nil)

(autoload 'my/org-capture-bubble-tea-template "my-org-autoloads" "\


\(fn LETTER DESC HEADINGS TEMPLATE &rest PROPERTIES)" nil nil)

(autoload 'my/org-agenda-visited-all-directories "my-org-autoloads" "\
Org agenda need to visted all files listed in `org-agenda-files'
to create the view, which is expensive. By default I will only list a
small portion of files to be searched.  This function searches all the
files in the org-directory to create the org-agenda view" t nil)

(autoload 'my/org-bubble-tea-get-end-of-play-time "my-org-autoloads" "\
After clocking in to record the start time of playing with bubble tea,
when clocking out, use this function to automatically update the table.

\(fn START)" nil nil)

(autoload 'my/org-toggle-org-emphasis-markers "my-org-autoloads" "\
toggle emphasis markers" t nil)

(autoload 'my/org-toggle-org-drawer "my-org-autoloads" "\
toggle hide drawer. This function is effective only after org 9.6." t nil)

(register-definition-prefixes "my-org-autoloads" '("my/toggle-org-settings-wrapper"))

;;;***

;;;### (autoloads nil "my-os-autoloads" "my-os-autoloads.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from my-os-autoloads.el

(autoload 'my/macos-cmd-w "my-os-autoloads" "\
If there is only one tab, close emacs, otherwise close one tab" t nil)

(autoload 'my/tty-setup "my-os-autoloads" nil nil nil)

;;;***

;;;### (autoloads nil "my-ui-autoloads" "my-ui-autoloads.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from my-ui-autoloads.el

(autoload 'my/delayed-startup-screen "my-ui-autoloads" "\
`org-agenda-list' is slow, don't run it immediately after startup" nil nil)

(autoload 'my/tab-bar-go-to-tab-macro "my-ui-autoloads" "\


\(fn NUMBER)" nil t)

(autoload 'my/set-scratch-directory "my-ui-autoloads" "\
After creating a new tab, the default buffer to be displayed is
scratch buffer whose directory is set to where emacs is initialized.
Change it to the directory of previous buffer where `tab-bar-new-tab'
is called.

\(fn OLD-FUN &rest ARGS)" nil nil)

(register-definition-prefixes "my-ui-autoloads" '("my/display-org-agenda-list"))

;;;***

;;;### (autoloads nil "my-utils-autoloads" "my-utils-autoloads.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from my-utils-autoloads.el

(autoload 'my/update-all-autoloads "my-utils-autoloads" nil t nil)

(autoload 'my/run-hook-once "my-utils-autoloads" "\
a wrapper to run a func on a hook only once

\(fn HOOK FUNC &rest ARGS)" nil t)

(autoload 'my/advise-at-once "my-utils-autoloads" "\
a wrapper to advise a func only once

\(fn FUNC ADVICE WHERE &rest PROPS)" nil t)

;;;***

(provide 'my-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; my-loaddefs.el ends here
