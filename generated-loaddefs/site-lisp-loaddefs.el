;;; site-lisp-loaddefs.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from ../site-lisp/consult-citre.el

(autoload 'consult-citre "../site-lisp/consult-citre" "\
Read a tag from minibuffer and jump to the tag.

(fn INITIAL)" t)
(register-definition-prefixes "../site-lisp/consult-citre" '("consult-citre-"))


;;; Generated autoloads from ../site-lisp/evil-ts.el

(register-definition-prefixes "../site-lisp/evil-ts" '("evil-ts-"))


;;; Generated autoloads from ../site-lisp/minuet.el

(autoload 'minuet-next-suggestion "../site-lisp/minuet" "\
Cycle to next suggestion." t)
(autoload 'minuet-previous-suggestion "../site-lisp/minuet" "\
Cycle to previous suggestion." t)
(autoload 'minuet-show-suggestion "../site-lisp/minuet" "\
Show code suggestion using overlay at point." t)
(autoload 'minuet-accept-suggestion "../site-lisp/minuet" "\
Accept the current overlay suggestion." t)
(autoload 'minuet-dismiss-suggestion "../site-lisp/minuet" "\
Dismiss the current overlay suggestion." t)
(autoload 'minuet-accept-suggestion-line "../site-lisp/minuet" "\
Accept only the first line of the current overlay suggestion." t)
(autoload 'minuet-completion-in-region "../site-lisp/minuet" "\
Complete code in region with LLM." t)
(autoload 'minuet-auto-suggestion-mode "../site-lisp/minuet" "\
Toggle automatic code suggestions.

When enabled, Minuet will automatically show suggestions while you type.

This is a minor mode.  If called interactively, toggle the
`Minuet-Auto-Suggestion mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `minuet-auto-suggestion-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "../site-lisp/minuet" '("minuet-"))


;;; Generated autoloads from ../site-lisp/vterm-repl.el

(autoload 'vtr-create-schema "../site-lisp/vterm-repl" "\
create a REPL schema.

The REPL session will be created via vterm. The schema includes three
functions, the function to start the repl, the function to send the
region and the corresponding operator, and the function to hide the
REPL window if it exists.

REPL-NAME is a string, REPL-CMD is a string, a form evaluated to a
string, or a function evaluated to a string. ARGS is a plist, the
following properties are supported:

:bracketed-paste-p whether send the string with bracketed paste mode,
the default value is nil.  You can change the behavior at run time by
setting the generated variable
`vtr*REPL-NAME-use-bracketed-paste-mode'.

:start-pattern the first string to send to the REPl before sending the
region. The default is ''.  You can change the behavior at run time by
setting the generated variable `vtr*REPL-NAME-start-pattern'.

:end-pattern the last string to send to the REPL after sending the
region. The default is '\\r'.  You can change the behavior at run time
by setting the generated variable `vtr*REPL-NAME-end-pattern'.

:str-process-func the function to process the string before sending it
to the REPL.  The default is `identity'. You can change the behavior
at run time by setting the generated variable
`vtr*REPL-NAME-str-process-func'.

(fn REPL-NAME REPL-CMD &rest ARGS)" nil t)
 (autoload #'vtr~aichat-start "vterm-repl" nil t)
 (autoload #'vtr~ipython-start "vterm-repl" nil t)
 (autoload #'vtr~radian-start "vterm-repl" nil t)


;;; Generated autoloads from ../site-lisp/vterm-repl-aider.el

 (autoload #'vtr~aider-start "vterm-repl-aider" nil t)
(register-definition-prefixes "../site-lisp/vterm-repl-aider" '("vtr-aider-"))

;;; End of scraped data

(provide 'site-lisp-loaddefs)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; site-lisp-loaddefs.el ends here
