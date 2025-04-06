;;; lib-loaddefs.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from ../lisp/lib/lib-apps.el

(autoload 'mg-open-html-with-xwidget "../lisp/lib/lib-apps" "\
open local file (html) using xwidget,
prefix with C-u to open the url with a new xwidget session

(fn URL NEW-SESSION)" t)
 (autoload #'mg-google-search-eww "lib-apps" nil t)
 (autoload #'mg-google-search-xwidget "lib-apps" nil t)
(defvar mg-refresh-xwidget-after-eval-python-mode nil "\
Non-nil if Mg-Refresh-Xwidget-After-Eval-Python mode is enabled.
See the `mg-refresh-xwidget-after-eval-python-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mg-refresh-xwidget-after-eval-python-mode'.")
(custom-autoload 'mg-refresh-xwidget-after-eval-python-mode "../lisp/lib/lib-apps" nil)
(autoload 'mg-refresh-xwidget-after-eval-python-mode "../lisp/lib/lib-apps" "\
After evaluating a python command, typically like

`python-shell-send-defun', `python-shell-send-region', refreshing the
xwidget browser. This is useful for interactive usage with web stuffs
like plotly.

This is a global minor mode.  If called interactively, toggle the
`Mg-Refresh-Xwidget-After-Eval-Python mode' mode.  If the prefix
argument is positive, enable the mode, and if it is zero or negative,
disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='mg-refresh-xwidget-after-eval-python-mode)'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(defvar mg-xwidget-side-window-mode nil "\
Non-nil if Mg-Xwidget-Side-Window mode is enabled.
See the `mg-xwidget-side-window-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mg-xwidget-side-window-mode'.")
(custom-autoload 'mg-xwidget-side-window-mode "../lisp/lib/lib-apps" nil)
(autoload 'mg-xwidget-side-window-mode "../lisp/lib/lib-apps" "\
`xwidget-webkit-browse-url' doesn't respect

`display-buffer-alist'.  This minor mode advises
`xwidget-webkit-browse-url' to make it respect such. This is helpful
for interactive plotting usage with python/R where you typically want
xwdiget to display plots at the side window.

This is a global minor mode.  If called interactively, toggle the
`Mg-Xwidget-Side-Window mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='mg-xwidget-side-window-mode)'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(defvar mg-xwidget-force-display-mode nil "\
Non-nil if Mg-Xwidget-Force-Display mode is enabled.
See the `mg-xwidget-force-display-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mg-xwidget-force-display-mode'.")
(custom-autoload 'mg-xwidget-force-display-mode "../lisp/lib/lib-apps" nil)
(autoload 'mg-xwidget-force-display-mode "../lisp/lib/lib-apps" "\
`xwidget-webkit-browse-url' won't display its buffer in current

frame when the xwidget session exists and no window is displaying that
session.  This minor mode advises `xwidget-webkit-browse-url' to
ensure such behavior. This is helpful for viewing web contents with
`mu4e', `notmuch', and `elfeed'

This is a global minor mode.  If called interactively, toggle the
`Mg-Xwidget-Force-Display mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable the
mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='mg-xwidget-force-display-mode)'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(autoload 'mg--elfeed-delete-window-after-kill-buffer "../lisp/lib/lib-apps" "\


(fn &rest ARGS)")
(autoload 'mg--elfeed-open-entry-via-xwidget "../lisp/lib/lib-apps" "\
if point is under a url, then open this url via `xwidget',
otherwise open the current visited elfeed entry via `xwidget'.  If
with a prefix \\[universal-argument] create a new `xwidget' session
otherwise use the existed one

(fn &optional NEW-SESSION)" t)
(autoload 'mg--elfeed-open-entry-via-eww "../lisp/lib/lib-apps" "\
if point is under a url, then open this url via `eww',
otherwise open the current visited elfeed entry via `eww'.  If
with a prefix \\[universal-argument] create a new `eww' session
otherwise use the existed one

(fn &optional NEW-SESSION)" t)
(autoload 'mg--pdf-midnight-mode-maybe "../lisp/lib/lib-apps")
(register-definition-prefixes "../lisp/lib/lib-apps" '("mg-"))


;;; Generated autoloads from ../lisp/lib/lib-colorscheme.el

(autoload 'mg--theme-set-dynamically "../lisp/lib/lib-colorscheme" "\
Select a theme at random from `mg-day-themes' or
`mg-night-themes', depending on the current time of day. The
environment variable `THEME_MODE' can override current time.
The time periods for day and night are specified by
`mg-day-to-night-o-clock' and `mg-night-to-day-o-clock',
respectively.")
(register-definition-prefixes "../lisp/lib/lib-colorscheme" '("mg-"))


;;; Generated autoloads from ../lisp/lib/lib-completion.el

(autoload 'mg-company-abort "../lisp/lib/lib-completion")
(autoload 'mg-company-completion-styles "../lisp/lib/lib-completion" "\


(fn CAPF-FN &rest ARGS)")


;;; Generated autoloads from ../lisp/lib/lib-elisp.el

(autoload 'mg-helpful-display-buffer "../lisp/lib/lib-elisp" "\
If a helpful buffer window is already opened, should use it,
don't occupy other window. Make sure it is a side window, such that
when you press q and want to close the help window), this window will
be completely removed, i.e. the window won't be displayed showing
other buffer.

(fn BUF)")
(autoload 'mg-helpful-lookup-symbl-at-point "../lisp/lib/lib-elisp" nil t)
(autoload 'mg-elisp-loop-up-symbol "../lisp/lib/lib-elisp" "\
Look up for the symbol under point, if region is active, use
        the selected region as the symbol

(fn BEG END)" t)
(autoload 'mg-elisp-setup "../lisp/lib/lib-elisp")
(autoload 'mg-lisp-indent-function "../lisp/lib/lib-elisp" "\
A replacement for `lisp-indent-function'.

Indents plists more sensibly. Adapted from
https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned

(fn INDENT-POINT STATE)")
(register-definition-prefixes "../lisp/lib/lib-elisp" '("mg-"))


;;; Generated autoloads from ../lisp/lib/lib-email.el

(autoload 'mg--notmuch-setup "../lisp/lib/lib-email" "\
Modified from `evil-collection-notmuch', but remove those keybindings I do not need. Especially for archiving.")
(register-definition-prefixes "../lisp/lib/lib-email" '("mg-"))


;;; Generated autoloads from ../lisp/lib/lib-evil.el

(autoload 'mg-save-excursion-before-indenting "../lisp/lib/lib-evil" "\


(fn ORIGIN-FN &rest ARGS)")
 (autoload #'mg-evil-apply-macro-line-by-line "lib-evil" nil t)
(autoload 'mg-define-and-bind-paren-text-object "../lisp/lib/lib-evil" "\


(fn KEY START-REGEX END-REGEX)" nil t)
(autoload 'mg-define-and-bind-local-paren-text-object "../lisp/lib/lib-evil" "\


(fn KEY START-REGEX END-REGEX HOOK)" nil t)
 (autoload #'mg-previous-SCM-conflict-marker "lib-evil" nil t)
 (autoload #'mg-next-SCM-conflict-marker "lib-evil" nil t)
 (autoload #'mg--evil-textobj-anyblock-inner-quote "lib-evil" nil t)
 (autoload #'mg--evil-apply-macro-line-by-line "lib-evil" nil t)
(register-definition-prefixes "../lisp/lib/lib-evil" '("mg-SCM-conflict-marker"))


;;; Generated autoloads from ../lisp/lib/lib-langs.el

 (autoload #'mg-send-region-to-ess "lib-langs" nil t)
 (autoload #'mg-send-region-to-python "lib-langs" nil t)
(autoload 'mg-ess-toggle-view-httpgd "../lisp/lib/lib-langs" "\
Display the httpgd buffer if not displayed. If the buffer is
displayed, close the window. If no httpgd buffer exists, ask to create
it." t)
(autoload 'mg-python-toggle-view-local-html "../lisp/lib/lib-langs" "\
Display the local html buffer if not displayed. If local html
buffer is displayed, close the window. If no local html buffer exists,
ask to create it." t)
(autoload 'mg-run-python "../lisp/lib/lib-langs" "\
Run python in project root that is dedicated to current buffer.
With an prefix \\[universal-argument], make this python session global
(not dedicated to any buffer).

(fn DEDICATED)" t)
(autoload 'mg-markdown-run-repl "../lisp/lib/lib-langs" "\
Run the REPL depending on the context (i.e. the language of the
code block)" t)
(autoload 'mg-markdown-hide-window "../lisp/lib/lib-langs" "\
Close the REPL window denpending on the context (i.e. the
language of the code block)." t)
 (autoload #'mg-markdown-send-region "lib-langs" nil t)
 (autoload #'mg-markdown-source-region "lib-langs" nil t)
(autoload 'mg-conda-activate "../lisp/lib/lib-langs" "\
Activate a conda environment.

(fn &optional PATH)" t)
(autoload 'mg-conda-deactivate "../lisp/lib/lib-langs" "\
Deactivate all the conda environments, including the base environment." t)
(autoload 'mg-python-venv-activate "../lisp/lib/lib-langs" "\
This command activates a python virtual environment.

(fn &optional PATH)" t)
(autoload 'mg-python-venv-deactivate "../lisp/lib/lib-langs" "\
This command deactivates the current python virtual environment." t)
(autoload 'mg-poetry-venv-activate "../lisp/lib/lib-langs" "\
This command activates a poetry virtual environment.

(fn &optional PATH)" t)
(autoload 'mg-poetry-venv-deactivate "../lisp/lib/lib-langs" "\
This command deactivates the current poetry virtual environment." t)
 (autoload #'yapf-format-buffer "lib-langs" nil t)
 (autoload #'black-format-buffer "lib-langs" nil t)
 (autoload #'sql-formatter-format-buffer "lib-langs" nil t)
(autoload 'mg-edit-src "../lisp/lib/lib-langs" "\
Edit the embedded code within a separate buffer." t)
(register-definition-prefixes "../lisp/lib/lib-langs" '("mg-"))


;;; Generated autoloads from ../lisp/lib/lib-langtools.el

(autoload 'mg-toggle-citre-eglot-capf "../lisp/lib/lib-langtools")
 (autoload #'mg-eldoc-buffer-dwim "lib-langtools" nil t)
(autoload 'mg-xref-move-in-original-src-macro "../lisp/lib/lib-langtools" "\
There can only be one xref buffer. That is, if you find
references of other symbol the previous one will be overwritten. The
official `xref-next-line' `xref-next-group' only allows you to move
the location in the src buffer when your point is in the xref buffer
window. This macro creates funcs that allow you to move current window
to next xref location.

(fn FUNC)" nil t)
(autoload 'mg-markdown-src-lsp-setup "../lisp/lib/lib-langtools" "\
eglot requires the buffer to be a file to be able to attach to
the lsp. Thus the indirect buffer created by `edit-indirect' needs to
be associated with a real file.")
(autoload 'mg-org-babel-lsp-setup "../lisp/lib/lib-langtools" "\
Support LANG in org source code block.

(fn LANG)" nil t)
(autoload 'mg-treesit-install-all-language-grammar "../lisp/lib/lib-langtools" nil t)
(autoload 'mg-formatter "../lisp/lib/lib-langtools" "\
If current LSP has a formatter, use it. Otherwise, use the
reformatter according to the `major-mode-reformatter-plist'" t)
(autoload 'mg-dape-start-or-continue "../lisp/lib/lib-langtools" "\
Try `dape-continue' and fall back to `dape'." t)
(autoload 'mg--dape-keymap-setup "../lisp/lib/lib-langtools")
(register-definition-prefixes "../lisp/lib/lib-langtools" '("major-mode-reformatter-plist" "mg-"))


;;; Generated autoloads from ../lisp/lib/lib-minibuffer.el

(autoload 'crm-indicator "../lisp/lib/lib-minibuffer" "\


(fn ARGS)")
(autoload 'mg-completion-in-region "../lisp/lib/lib-minibuffer" "\


(fn &rest ARGS)")
 (autoload #'mg-evil-delete-in-wgrep "lib-minibuffer" nil t)


;;; Generated autoloads from ../lisp/lib/lib-misc.el

(autoload 'call-command-at-project-root "../lisp/lib/lib-misc" "\
call command at project root, if no root is found, open at the default-directory

(fn ORIG-FUN &rest ARGS)")
(autoload 'wrap-command-at-project-root "../lisp/lib/lib-misc" "\
create a new command that calls orig-cmd at project root, if no root is found, open at the default-directory

(fn ORIG-CMD)")
(autoload 'mg-project-root-or-default-dir "../lisp/lib/lib-misc" "\
If a project root is found, return it. Otherwise return `default-directory'.")
(autoload 'mg-dired-find-file-other-tab "../lisp/lib/lib-misc" "\
In Dired, visit this file or directory in another window." t)
(autoload 'mg-ibuffer-vc-setup "../lisp/lib/lib-misc")
(autoload 'mg--dired-subtree-toggle-nerd-icons "../lisp/lib/lib-misc")
(autoload 'mg--vterm-override-evil-collection "../lisp/lib/lib-misc" "\


(fn MODE _ &rest _)")
(register-definition-prefixes "../lisp/lib/lib-misc" '("mg--dired-subtree-add-nerd-icons"))


;;; Generated autoloads from ../lisp/lib/lib-org.el

(autoload 'mg-org-capture-bubble-tea-template "../lisp/lib/lib-org" "\


(fn LETTER DESC HEADINGS TEMPLATE &rest PROPERTIES)")
(autoload 'mg-org-agenda-visited-all-directories "../lisp/lib/lib-org" "\
Org agenda need to visted all files listed in `org-agenda-files'
to create the view, which is expensive. By default I will only list a
small portion of files to be searched.  This function searches all the
files in the org-directory to create the org-agenda view" t)
(autoload 'mg-org-bubble-tea-get-end-of-play-time "../lisp/lib/lib-org" "\
After clocking in to record the start time of playing with bubble tea,
when clocking out, use this function to automatically update the table.

(fn START)")
(autoload 'mg-exclude-org-agenda-buffers-from-recentf "../lisp/lib/lib-org" "\
Prevent `org-agenda' buffers from polluting recentf list.

(fn OLD-FN &rest ARGS)")
(autoload 'mg-reload-org-agenda-buffers "../lisp/lib/lib-org" "\
`org-agenda' creates incomplete `org-mode' buffers to boost its startup speed. Reload those buffers
after `org-agenda' has finalized.")
(autoload 'mg-org-indent-maybe-h "../lisp/lib/lib-org" "\
Indent the current item (header or item), if possible.
Made for `org-tab-first-hook' in evil-mode." t)
(autoload 'mg-org-yas-expand-maybe-h "../lisp/lib/lib-org" "\
Expand a yasnippet snippet, if trigger exists at point or region is active.
Made for `org-tab-first-hook'.")
(autoload 'mg-org-toggle-org-emphasis-markers "../lisp/lib/lib-org" "\
toggle emphasis markers" t)
(autoload 'mg-org-toggle-org-drawer "../lisp/lib/lib-org" "\
toggle hide drawer. This function is effective only after org 9.6." t)
(register-definition-prefixes "../lisp/lib/lib-org" '("mg-toggle-org-settings-wrapper"))


;;; Generated autoloads from ../lisp/lib/lib-os.el

(autoload 'mg-macos-cmd-w "../lisp/lib/lib-os" "\
If there is only one tab, close emacs, otherwise close one tab" t)
(autoload 'mg-tty-setup "../lisp/lib/lib-os")
(autoload 'mg--server-setup "../lisp/lib/lib-os")


;;; Generated autoloads from ../lisp/lib/lib-ui.el

(autoload 'mg-display-truncation-and-wrap-indicator-as-whitespace "../lisp/lib/lib-ui")
(autoload 'mg-tab-bar-go-to-tab-macro "../lisp/lib/lib-ui" "\


(fn NUMBER)" nil t)
(autoload 'mg-set-scratch-directory "../lisp/lib/lib-ui" "\
After creating a new tab, the default buffer to be displayed is
scratch buffer whose directory is set to where emacs is initialized.
Change it to the directory of previous buffer where `tab-bar-new-tab'
is called.

(fn OLD-FUN &rest ARGS)")
(autoload 'mg--font-set-small-mono-font "../lisp/lib/lib-ui" "\
Set the default font to a smaller sized font for current buffer.")
(autoload 'mg--font-set-small-variable-font "../lisp/lib/lib-ui" "\
Set the default font to a smaller sized font for current buffer.")
(autoload 'mg--welcome-screen-mode "../lisp/lib/lib-ui")
(autoload 'mg-refresh-verses "../lisp/lib/lib-ui" "\
refresh verses in the scratch buffer" t)
(register-definition-prefixes "../lisp/lib/lib-ui" '("mg-"))


;;; Generated autoloads from ../lisp/lib/lib-utils.el

(autoload 'mg-update-all-autoloads "../lisp/lib/lib-utils" "\
Update all autoloads in the AUTOLOADS-DIR into the AUTOLOADS-FILE.
If AUTOLOADS-DIR is nil, use `mg-autoloads-dir'. If AUTOLOADS-FILE is
nil, use `mg-autoloads-file'.

(fn &optional AUTOLOADS-DIR AUTOLOADS-FILE)" t)
(autoload 'mg-update-site-lisp-autoloads "../lisp/lib/lib-utils" nil t)
(autoload 'mg-run-hook-once "../lisp/lib/lib-utils" "\
a wrapper to run a function (can be named or lambda) on a hook or
a list of hooks only once.  When HOOKS is a list, only run FUNC once
on the first hook. The rest hook will not run FUNC.

ARGS is a plist which takes the following keys:

:func-name a string which is the name of FUNC. If FUNC is a lambda,
must be provided. Can be omitted if FUNC is a symbol.

:func-args a list which is the rest args passed to FUNC.

(fn HOOKS FUNC &rest ARGS)" nil t)
(autoload 'mg-advise-at-once "../lisp/lib/lib-utils" "\
a wrapper to advise a func only once

(fn FUNC ADVICE WHERE &rest PROPS)" nil t)
(autoload 'mg-turn-off-mode "../lisp/lib/lib-utils" "\
Create a function to turn off MODE. Useful for attaching on some
hooks that will turn off MODE locally.

(fn MODE)" nil t)
(autoload 'mg-setq-locally "../lisp/lib/lib-utils" "\
Create a function that sets a local value to a variable (VAR)
called VAL. This function is particularly useful for setting variables
locally in certain hooks.

For setting a simple VAL to VAR in multiple modes, use
`mg-setq-locally'. In case you want to set a complex VAL to VAR in a
single mode, use `mg-setq-on-hook'. You might wonder why you shouldn't
simply use (add-hook 'foo-hook (lambda () FORM)? This is because, upon
running \\[describe-variable] `foo-hook RET', you'll find the lambda
functions unreadable. Using a named function in a hook, however, makes
the hook more elegantly described, which proves to be useful for
debugging purposes if you desire to scrutinize a hook value.

(fn VAR VAL)" nil t)
(autoload 'mg-setq-on-hook "../lisp/lib/lib-utils" "\
Create a function that sets VAR to VAL on a HOOK.

If you want to set VAR to a simple VAL in multiple modes, use
`mg-setq-locally'. However, if you want to set VAR to a complex VAL in
only one mode, use `mg-setq-on-hook'. Why not directly use (add-hook
'foo-hook (lambda () FORM))'? Well, when you try to describe the
variable with \\[describe-variable] `foo-hook RET', those lambda
functions become unreadable. Using a named function in a hook results
in much nicer description of the hook. This is particularly helpful
for debugging purposes when you want to examine a hook value.

(fn HOOK VAR VAL)" nil t)
(autoload 'mg--load-packages-incrementally-setup "../lisp/lib/lib-utils" "\
Set up a idle timer to start idly load packages.")
(register-definition-prefixes "../lisp/lib/lib-utils" '("mg-"))


;;; Generated autoloads from ../lisp/lib/lib-vcs.el

(autoload 'mg-project-todos "../lisp/lib/lib-vcs" "\
Find `hl-todo--regex' items in project using `consult-ripgrep'" t)
 (autoload #'mg-project-magit "lib-vcs" nil t)

;;; End of scraped data

(provide 'lib-loaddefs)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; lib-loaddefs.el ends here
