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

(autoload 'minuet-completion-in-region "../site-lisp/minuet" "\
Complete code in region with LLM." t)
(register-definition-prefixes "../site-lisp/minuet" '("minuet-"))


;;; Generated autoloads from ../site-lisp/vterm-repl.el

(autoload 'vtr-create-schema "../site-lisp/vterm-repl" "\
create a REPL schema.

The REPL session will be created via vterm. The schema includes three
functions, the function to start the repl, the function to send the
region and the corresponding operator, and the function to hide the
REPL window if it exists.

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
`my*REPL-NAME-str-process-func'.

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