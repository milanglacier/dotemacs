;;; my-loaddefs.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from my-apps-autoloads.el

(autoload 'my/open-html-with-xwidget "my-apps-autoloads" "\
open local file (html) using xwidget,
prefix with C-u to open the url with a new xwidget session

(fn URL NEW-SESSION)" t)
 (autoload #'my/google-search-eww "my-apps-autoloads" nil t)
 (autoload #'my/google-search-xwidget "my-apps-autoloads" nil t)
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

This is a global minor mode.  If called interactively, toggle the
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

(fn &optional ARG)" t)
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

This is a global minor mode.  If called interactively, toggle the
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

(fn &optional ARG)" t)
(defvar my/xwidget-force-display-mode nil "\
Non-nil if My/Xwidget-Force-Display mode is enabled.
See the `my/xwidget-force-display-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `my/xwidget-force-display-mode'.")
(custom-autoload 'my/xwidget-force-display-mode "my-apps-autoloads" nil)
(autoload 'my/xwidget-force-display-mode "my-apps-autoloads" "\
`xwidget-webkit-browse-url' won't display its buffer in current

frame when the xwidget session exists and no window is displaying that
session.  This minor mode advises `xwidget-webkit-browse-url' to
ensure such behavior. This is helpful for viewing web contents with
`mu4e', `notmuch', and `elfeed'

This is a global minor mode.  If called interactively, toggle the
`My/Xwidget-Force-Display mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='my/xwidget-force-display-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(autoload 'my:elfeed-delete-window-after-kill-buffer "my-apps-autoloads" "\


(fn &rest ARGS)")
(autoload 'my:elfeed-open-entry-via-xwidget "my-apps-autoloads" "\
if point is under a url, then open this url via `xwidget',
otherwise open the current visited elfeed entry via `xwidget'.  If
with a prefix \\[universal-argument] create a new `xwidget' session
otherwise use the existed one

(fn &optional NEW-SESSION)" t)
(autoload 'my:elfeed-open-entry-via-eww "my-apps-autoloads" "\
if point is under a url, then open this url via `eww',
otherwise open the current visited elfeed entry via `eww'.  If
with a prefix \\[universal-argument] create a new `eww' session
otherwise use the existed one

(fn &optional NEW-SESSION)" t)
(autoload 'my:pdf-midnight-mode-maybe "my-apps-autoloads")
 (autoload #'my~aichat-start "my-apps-autoloads" nil t)
 (autoload #'my~ipython-start "my-apps-autoloads" nil t)
 (autoload #'my~radian-start "my-apps-autoloads" nil t)
(register-definition-prefixes "my-apps-autoloads" '("my%create-vterm-repl-schema" "my/"))


;;; Generated autoloads from my-colorscheme-autoloads.el

(autoload 'my:theme-set-dynamically "my-colorscheme-autoloads" "\
Select a theme at random from `my$day-themes' or
`my$night-themes', depending on the current time of day. The
environment variable `CURRENT_BACKGROUND' can override current time.
The time periods for day and night are specified by
`my$day-to-night-o-clock' and `my$night-to-day-o-clock',
respectively.")
(register-definition-prefixes "my-colorscheme-autoloads" '("my$" "my:"))


;;; Generated autoloads from my-completion-autoloads.el

(autoload 'my/company-abort "my-completion-autoloads")
(autoload 'my/company-completion-styles "my-completion-autoloads" "\


(fn CAPF-FN &rest ARGS)")


;;; Generated autoloads from my-elisp-autoloads.el

(autoload 'my/helpful-display-buffer "my-elisp-autoloads" "\
If a helpful buffer window is already opened, should use it,
don't occupy other window. Make sure it is a side window, such that
when you press q and want to close the help window), this window will
be completely removed, i.e. the window won't be displayed showing
other buffer.

(fn BUF)")
(autoload 'my/helpful-lookup-symbl-at-point "my-elisp-autoloads" nil t)
(autoload 'my/elisp-loop-up-symbol "my-elisp-autoloads" "\
Look up for the symbol under point, if region is active, use
        the selected region as the symbol

(fn BEG END)" t)
(autoload 'my/elisp-setup "my-elisp-autoloads")
(autoload 'my/lisp-indent-function "my-elisp-autoloads" "\
A replacement for `lisp-indent-function'.

Indents plists more sensibly. Adapted from
https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned

(fn INDENT-POINT STATE)")
(register-definition-prefixes "my-elisp-autoloads" '("my&emacs-lisp--face" "my/emacs-lisp-outline-level" "my:emacs-lisp-highlight-vars-and-faces"))


;;; Generated autoloads from my-email-autoloads.el

(autoload 'my:mu4e-enter-func "my-email-autoloads" "\


(fn USE-ORG-MSG-MODE &optional ADDITIONAL-FUNC)")
(autoload 'my:mu4e-leave-func "my-email-autoloads" "\


(fn &optional ADDITIONAL-FUNC)")
(autoload 'my:mu4e-match-func "my-email-autoloads" "\


(fn PREFIX)")
(autoload 'my:mu4e-open-link-via-eww "my-email-autoloads" "\
If point is on a link, open this link via `eww'. Otherwise open
this email via `eww'

(fn MSG)")
(defvar my~mu4e-thread-folding-mode nil "\
Non-nil if My~Mu4e-Thread-Folding mode is enabled.
See the `my~mu4e-thread-folding-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `my~mu4e-thread-folding-mode'.")
(custom-autoload 'my~mu4e-thread-folding-mode "my-email-autoloads" nil)
(autoload 'my~mu4e-thread-folding-mode "my-email-autoloads" "\
Enable thread folding for mu4e.

This is a global minor mode.  If called interactively, toggle the
`My~Mu4e-Thread-Folding mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='my~mu4e-thread-folding-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "my-email-autoloads" '("my$mu4e-" "my:mu4e-" "my~mu4e-"))


;;; Generated autoloads from my-evil-autoloads.el

(autoload 'my/save-excursion-before-indenting "my-evil-autoloads" "\


(fn ORIGIN-FN &rest ARGS)")
 (autoload #'my/evil-apply-macro-line-by-line "my-evil-autoloads" nil t)
(autoload 'my/define-and-bind-paren-text-object "my-evil-autoloads" "\


(fn KEY START-REGEX END-REGEX)" nil t)
(autoload 'my/define-and-bind-local-paren-text-object "my-evil-autoloads" "\


(fn KEY START-REGEX END-REGEX HOOK)" nil t)
 (autoload #'my/previous-SCM-conflict-marker "my-evil-autoloads" nil t)
 (autoload #'my/next-SCM-conflict-marker "my-evil-autoloads" nil t)
 (autoload #'my:evil-textobj-anyblock-inner-quote "my-evil-autoloads" nil t)
 (autoload #'my:evil-apply-macro-line-by-line "my-evil-autoloads" nil t)
(register-definition-prefixes "my-evil-autoloads" '("my/SCM-conflict-marker"))


;;; Generated autoloads from my-langs-autoloads.el

 (autoload #'my/send-region-to-ess "my-langs-autoloads" nil t)
 (autoload #'my/send-region-to-python "my-langs-autoloads" nil t)
(autoload 'my/ess-toggle-view-httpgd "my-langs-autoloads" "\
Display the httpgd buffer if not displayed. If the buffer is
displayed, close the window. If no httpgd buffer exists, ask to create
it." t)
(autoload 'my/python-toggle-view-local-html "my-langs-autoloads" "\
Display the local html buffer if not displayed. If local html
buffer is displayed, close the window. If no local html buffer exists,
ask to create it." t)
(autoload 'my/run-python "my-langs-autoloads" "\
Run python in project root that is dedicated to current buffer.
With an prefix \\[universal-argument], make this python session global
(not dedicated to any buffer).

(fn DEDICATED)" t)
(autoload 'my/markdown-run-repl "my-langs-autoloads" "\
Run the REPL depending on the context (i.e. the language of the
code block)" t)
 (autoload #'my/markdown-send-region "my-langs-autoloads" nil t)
(autoload 'my~conda-activate "my-langs-autoloads" "\
Activate a conda environment.

(fn &optional PATH)" t)
(autoload 'my~conda-deactivate "my-langs-autoloads" "\
Deactivate all the conda environments, including the base environment." t)
(autoload 'my~python-venv-activate "my-langs-autoloads" "\
This command activates a python virtual environment.

(fn &optional PATH)" t)
(autoload 'my~python-venv-deactivate "my-langs-autoloads" "\
This command deactivates the current python virtual environment." t)
(autoload 'my~poetry-venv-activate "my-langs-autoloads" "\
This command activates a poetry virtual environment.

(fn &optional PATH)" t)
(autoload 'my~poetry-venv-deactivate "my-langs-autoloads" "\
This command deactivates the current poetry virtual environment." t)
 (autoload #'yapf-format-buffer "my-langs-autoloads" nil t)
 (autoload #'black-format-buffer "my-langs-autoloads" nil t)
 (autoload #'sql-formatter-format-buffer "my-langs-autoloads" nil t)
(register-definition-prefixes "my-langs-autoloads" '("my$" "my/"))


;;; Generated autoloads from my-langtools-autoloads.el

(autoload 'my/toggle-citre-eglot-capf "my-langtools-autoloads")
 (autoload #'my~codeium-completion "my-langtools-autoloads" nil t)
 (autoload #'my/eldoc-buffer-dwim "my-langtools-autoloads" nil t)
(autoload 'my/xref-move-in-original-src-macro "my-langtools-autoloads" "\
There can only be one xref buffer. That is, if you find
references of other symbol the previous one will be overwritten. The
official `xref-next-line' `xref-next-group' only allows you to move
the location in the src buffer when your point is in the xref buffer
window. This macro creates funcs that allow you to move current window
to next xref location.

(fn FUNC)" nil t)
(autoload 'my/markdown-src-lsp-setup "my-langtools-autoloads" "\
eglot requires the buffer to be a file to be able to attach to
the lsp. Thus the indirect buffer created by `edit-indirect' needs to
be associated with a real file.")
(autoload 'my/org-babel-lsp-setup "my-langtools-autoloads" "\
Support LANG in org source code block.

(fn LANG)" nil t)
(autoload 'my~treesit-install-all-language-grammar "my-langtools-autoloads" nil t)
(autoload 'my~formatter "my-langtools-autoloads" "\
If current LSP has a formatter, use it. Otherwise, use the
reformatter according to the `major-mode-reformatter-plist'" t)
(autoload 'my~dape-start-or-continue "my-langtools-autoloads" "\
Try `dape-continue' and fall back to `dape'." t)
(autoload 'my:dape-keymap-setup "my-langtools-autoloads")
(register-definition-prefixes "my-langtools-autoloads" '("major-mode-reformatter-plist"))


;;; Generated autoloads from my-minibuffer-autoloads.el

(autoload 'crm-indicator "my-minibuffer-autoloads" "\


(fn ARGS)")
(autoload 'my/completion-in-region "my-minibuffer-autoloads" "\


(fn &rest ARGS)")
 (autoload #'my/evil-delete-in-wgrep "my-minibuffer-autoloads" nil t)


;;; Generated autoloads from my-misc-autoloads.el

(autoload 'call-command-at-project-root "my-misc-autoloads" "\
call command at project root, if no root is found, open at the default-directory

(fn ORIG-FUN &rest ARGS)")
(autoload 'wrap-command-at-project-root "my-misc-autoloads" "\
create a new command that calls orig-cmd at project root, if no root is found, open at the default-directory

(fn ORIG-CMD)")
(autoload 'my/project-root-or-default-dir "my-misc-autoloads" "\
If a project root is found, return it. Otherwise return `default-directory'.")
(autoload 'my~dired-find-file-other-tab "my-misc-autoloads" "\
In Dired, visit this file or directory in another window." t)
(autoload 'my/ibuffer-vc-setup "my-misc-autoloads")
(autoload 'my:dired-subtree-toggle-nerd-icons "my-misc-autoloads")
(register-definition-prefixes "my-misc-autoloads" '("my:dired-subtree-add-nerd-icons"))


;;; Generated autoloads from my-org-autoloads.el

(autoload 'my/org-capture-bubble-tea-template "my-org-autoloads" "\


(fn LETTER DESC HEADINGS TEMPLATE &rest PROPERTIES)")
(autoload 'my/org-agenda-visited-all-directories "my-org-autoloads" "\
Org agenda need to visted all files listed in `org-agenda-files'
to create the view, which is expensive. By default I will only list a
small portion of files to be searched.  This function searches all the
files in the org-directory to create the org-agenda view" t)
(autoload 'my/org-bubble-tea-get-end-of-play-time "my-org-autoloads" "\
After clocking in to record the start time of playing with bubble tea,
when clocking out, use this function to automatically update the table.

(fn START)")
(autoload 'my/exclude-org-agenda-buffers-from-recentf "my-org-autoloads" "\
Prevent `org-agenda' buffers from polluting recentf list.

(fn OLD-FN &rest ARGS)")
(autoload 'my/reload-org-agenda-buffers "my-org-autoloads" "\
`org-agenda' creates incomplete `org-mode' buffers to boost its startup speed. Reload those buffers
after `org-agenda' has finalized.")
(autoload 'my/org-indent-maybe-h "my-org-autoloads" "\
Indent the current item (header or item), if possible.
Made for `org-tab-first-hook' in evil-mode." t)
(autoload 'my/org-yas-expand-maybe-h "my-org-autoloads" "\
Expand a yasnippet snippet, if trigger exists at point or region is active.
Made for `org-tab-first-hook'.")
(autoload 'my/org-toggle-org-emphasis-markers "my-org-autoloads" "\
toggle emphasis markers" t)
(autoload 'my/org-toggle-org-drawer "my-org-autoloads" "\
toggle hide drawer. This function is effective only after org 9.6." t)
(register-definition-prefixes "my-org-autoloads" '("my/toggle-org-settings-wrapper"))


;;; Generated autoloads from my-os-autoloads.el

(autoload 'my/macos-cmd-w "my-os-autoloads" "\
If there is only one tab, close emacs, otherwise close one tab" t)
(autoload 'my/tty-setup "my-os-autoloads")


;;; Generated autoloads from my-ui-autoloads.el

(autoload 'my/display-truncation-and-wrap-indicator-as-whitespace "my-ui-autoloads")
(autoload 'my/tab-bar-go-to-tab-macro "my-ui-autoloads" "\


(fn NUMBER)" nil t)
(autoload 'my/set-scratch-directory "my-ui-autoloads" "\
After creating a new tab, the default buffer to be displayed is
scratch buffer whose directory is set to where emacs is initialized.
Change it to the directory of previous buffer where `tab-bar-new-tab'
is called.

(fn OLD-FUN &rest ARGS)")
(autoload 'my:font-set-small-mono-font "my-ui-autoloads" "\
Set the default font to a smaller sized font for current buffer.")
(autoload 'my:font-set-small-variable-font "my-ui-autoloads" "\
Set the default font to a smaller sized font for current buffer.")
(defvar my~show-verses-at-startup-mode nil "\
Non-nil if My~Show-Verses-At-Startup mode is enabled.
See the `my~show-verses-at-startup-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `my~show-verses-at-startup-mode'.")
(custom-autoload 'my~show-verses-at-startup-mode "my-ui-autoloads" nil)
(autoload 'my~show-verses-at-startup-mode "my-ui-autoloads" "\
show verses at the startup screen.

This is a global minor mode.  If called interactively, toggle the
`My~Show-Verses-At-Startup mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='my~show-verses-at-startup-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(autoload 'my~refresh-verses "my-ui-autoloads" "\
refresh verses in the scratch buffer" t)
(register-definition-prefixes "my-ui-autoloads" '("my$" "my&welcome-screen-action-button" "my:"))


;;; Generated autoloads from my-utils-autoloads.el

(autoload 'my/update-all-autoloads "my-utils-autoloads" "\
Update all autoloads in the AUTOLOADS-DIR into the AUTOLOADS-FILE.
If AUTOLOADS-DIR is nil, use `my/autoloads-dir'. If AUTOLOADS-FILE is
nil, use `my/autoloads-file'.

(fn &optional AUTOLOADS-DIR AUTOLOADS-FILE)" t)
(autoload 'my/update-site-lisp-autoloads "my-utils-autoloads" nil t)
(autoload 'my/run-hook-once "my-utils-autoloads" "\
a wrapper to run a func on a hook only once

(fn HOOK FUNC &rest ARGS)" nil t)
(autoload 'my/advise-at-once "my-utils-autoloads" "\
a wrapper to advise a func only once

(fn FUNC ADVICE WHERE &rest PROPS)" nil t)
(autoload 'my/turn-off-mode "my-utils-autoloads" "\
Create a function to turn off MODE. Useful for attaching on some
hooks that will turn off MODE locally.

(fn MODE)" nil t)
(autoload 'my/setq-locally "my-utils-autoloads" "\
Create a function that sets a local value to a variable (VAR)
called VAL. This function is particularly useful for setting variables
locally in certain hooks.

For setting a simple VAL to VAR in multiple modes, use
`my/setq-locally'. In case you want to set a complex VAL to VAR in a
single mode, use `my/setq-on-hook'. You might wonder why you shouldn't
simply use (add-hook 'foo-hook (lambda () FORM)? This is because, upon
running \\[describe-variable] `foo-hook RET', you'll find the lambda
functions unreadable. Using a named function in a hook, however, makes
the hook more elegantly described, which proves to be useful for
debugging purposes if you desire to scrutinize a hook value.

(fn VAR VAL)" nil t)
(autoload 'my/setq-on-hook "my-utils-autoloads" "\
Create a function that sets VAR to VAL on a HOOK.

If you want to set VAR to a simple VAL in multiple modes, use
`my/setq-locally'. However, if you want to set VAR to a complex VAL in
only one mode, use `my/setq-on-hook'. Why not directly use (add-hook
'foo-hook (lambda () FORM))'? Well, when you try to describe the
variable with \\[describe-variable] `foo-hook RET', those lambda
functions become unreadable. Using a named function in a hook results
in much nicer description of the hook. This is particularly helpful
for debugging purposes when you want to examine a hook value.

(fn HOOK VAR VAL)" nil t)
(autoload 'my:load-packages-incrementally-setup "my-utils-autoloads" "\
Set up a idle timer to start idly load packages.")
(register-definition-prefixes "my-utils-autoloads" '("my$" "my%call-func-respect-blocklist" "my:load-packages-incrementally"))


;;; Generated autoloads from my-vcs-autoloads.el

(autoload 'my/project-todos "my-vcs-autoloads" "\
Find `hl-todo--regex' items in project using `consult-ripgrep'" t)
 (autoload #'my~project-magit "my-vcs-autoloads" nil t)

;;; End of scraped data

(provide 'my-loaddefs)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; my-loaddefs.el ends here
