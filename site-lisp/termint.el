;;; termint.el --- Run REPLs in a terminal backend -*- lexical-binding: t; -*-

;; Author: Milan Glacier <dev@milanglacier.com>
;; Maintainer: Milan Glacier <dev@milanglacier.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29"))

;;; Commentary:

;; This package offers macros and functions for creating and managing
;; REPL sessions within a terminal emulator backend (term, eat, or
;; vterm).  It facilitates the creation of custom REPL commands
;; tailored to each defined REPL, with features including starting
;; session, sending code, and hiding REPL windows.  This is useful for
;; integrating terminal-based REPLs with Emacs efficiently.

;;; Code:

(defgroup termint nil
    "Group for termint."
    :group 'tools)

(defcustom termint-backend 'term
    "The backend to use for REPL sessions."
    :type '(choice (const :tag "eat" eat)
                   (const :tag "vterm" vterm)
                   (const :tag "term" term)))

(defmacro termint-define (repl-name repl-cmd &rest args)
    "Define a REPL schema.

The REPL session will be created via `termint-backend'.  The schema
includes three functions: one to start the REPL, one to send the
region and corresponding Evil operator (for Evil users), and one to
hide the REPL window if it exists.  A keymap, `termint-REPL-NAME-map', is
also included for these commands.

REPL-NAME is a string, REPL-CMD is a string, a form evaluated to a
string, or a function evaluated to a string.  ARGS is a plist, the
following properties are supported:

:bracketed-paste-p whether send the string with bracketed paste mode,
the default value is nil.  You can change the behavior at run time by
setting the generated variable
`termint-REPL-NAME-use-bracketed-paste-mode'.

:start-pattern the first string to send to the REPl before sending the
region.  The default is ''.  You can change the behavior at run time by
setting the generated
variable`termint-REPL-NAME-start-pattern'.  Additionally, the value can be
a plist with two attributes: `:single-line' for specifying the string
in single-line scenarios.`:multi-lines' for defining the string in
multi-line contexts.

:end-pattern the last string to send to the REPL after sending the
region.  The default is '\\r'.  You can change the behavior at run time
by setting the generated variable
`termint-REPL-NAME-end-pattern'.  Additionally the value can be a plist
with two attributes: `:single-line' for specifying the string in
single-line scenarios, and `:multi-lines' for defining the string in
multi-line contexts.

:str-process-func the function to process the string before sending it
to the REPL.  The default is `identity'.  You can change the behavior
at run time by setting the generated variable
`termint-REPL-NAME-str-process-func'.

:source-func the function to source the code content to the REPL.  A
common approach involves writing the input string to a temporary file,
then returning a string that sources this file.  The exact \"sourcing\"
syntax depends on the target programming language."

    (let ((start-func-name (intern (concat "termint-" repl-name "-start")))
          (send-region-func-name (intern (concat "termint-" repl-name "-send-region")))
          (send-region-operator-name (intern (concat "termint-" repl-name "-send-region-operator")))
          (source-region-func-name (intern (concat "termint-" repl-name "-source-region")))
          (source-region-operator-name (intern (concat "termint-" repl-name "-source-region-operator")))
          (send-string-func-name (intern (concat "termint-" repl-name "-send-string")))
          (hide-window-func-name (intern (concat "termint-" repl-name "-hide-window")))
          (keymap-name (intern (concat "termint-" repl-name "-map")))
          (bracketed-paste-p (plist-get args :bracketed-paste-p))
          (start-pattern (or (plist-get args :start-pattern) ""))
          (end-pattern (or (plist-get args :end-pattern) "\r"))
          (str-process-func (or (plist-get args :str-process-func) ''identity))
          (source-func (or (plist-get args :source-func) ''identity))
          (repl-cmd-name (intern (concat "termint-" repl-name "-cmd")))
          (str-process-func-name (intern (concat "termint-" repl-name "-str-process-func")))
          (source-func-name (intern (concat "termint-" repl-name "-source-func")))
          (bracketed-paste-p-name (intern (concat "termint-" repl-name "-use-bracketed-paste-mode")))
          (start-pattern-name (intern (concat "termint-" repl-name "-start-pattern")))
          (end-pattern-name (intern (concat "termint-" repl-name "-end-pattern"))))

        `(progn

             (require 'eat nil t)
             (require 'vterm nil t)
             (require 'term nil t)

             (defvar ,repl-cmd-name ,repl-cmd
                 ,(format "The shell command for the %s REPL." repl-name))

             (defvar ,str-process-func-name ,str-process-func
                 ,(format "The function to process the string before sending it to the %s REPL." repl-name))

             (defvar ,source-func-name ,source-func
                 ,(format "The function to source the code content for the %s REPL." repl-name))

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
the buffer selected (or created). With a numeric prefix arg, create or
switch to the session with that number as a suffix."
                   repl-name repl-name)
                 (interactive "P")
                 (let* ((repl-buffer-name (format "*%s*" ,repl-name))
                        (repl-shell (if (functionp ,repl-cmd-name)
                                            (funcall ,repl-cmd-name)
                                        ,repl-cmd-name))
                        (eat-buffer-name repl-buffer-name)
                        (eat-shell repl-shell)
                        (vterm-buffer-name repl-buffer-name)
                        (vterm-shell repl-shell)
                        (term-buffer-name
                         (if arg
                                 (format "%s<%d>" repl-buffer-name arg)
                             repl-buffer-name)))
                     (pcase termint-backend
                         ('eat (eat nil arg))
                         ('vterm (vterm arg))
                         ('term
                          (if (get-buffer term-buffer-name)
                                  (pop-to-buffer term-buffer-name)
                              (let* ((shell-list (split-string-shell-command repl-shell))
                                     (shell-cmd (car shell-list))
                                     (shell-args (cdr shell-list))
                                     (term-buffer (get-buffer-create term-buffer-name)))
                                  (with-current-buffer term-buffer
                                      (term-mode)
                                      (term-exec term-buffer term-buffer-name shell-cmd nil shell-args)
                                      (term-char-mode))
                                  (pop-to-buffer term-buffer)))))))


             (defun ,send-region-func-name (beg end &optional session)
                 ,(format
                   "Send the region delimited by BEG and END to inferior %s.
With numeric prefix argument, send region to the process associated
with that number." repl-name)
                 (interactive "r\nP")
                 (let ((str (buffer-substring-no-properties beg end)))
                     (,send-string-func-name str session)))

             (defun ,source-region-func-name (beg end &optional session)
                 ,(format
                   "Source the region delimited by BEG and END to inferior %s.
With numeric prefix argument, send region to the process associated
with that number." repl-name)
                 (interactive "r\nP")
                 (let* ((str (buffer-substring-no-properties beg end))
                        (str (funcall ,source-func-name str)))
                     (,send-string-func-name str session)))

             (defun ,send-string-func-name (string &optional session)
                 ,(format
                   "Send the string to inferior %s. When invoked
interactively, prompt the user for input in the minibuffer.  With
numeric prefix argument, send region to the process associated with
that number." repl-name)
                 (interactive "sinput your command: \nP")
                 (let* ((repl-buffer-name
                         (if session
                                 (format "*%s*<%d>" ,repl-name session)
                             (format "*%s*" ,repl-name)))
                        (send-string
                         (pcase termint-backend
                             ('eat (lambda (str) (eat--send-string nil str)))
                             ('vterm (lambda (str) (vterm-send-string str)))
                             ('term (lambda (str) (term-send-raw-string str)))))
                        (multi-lines-p (string-match-p "\n" string))
                        (bracketed-paste-start "\e[200~")
                        (bracketed-paste-end "\e[201~")
                        (start-pattern (if (stringp ,start-pattern-name) ,start-pattern-name
                                           (if multi-lines-p
                                                   (plist-get ,start-pattern-name :multi-lines)
                                               (plist-get ,start-pattern-name :single-line))))
                        (end-pattern (if (stringp ,end-pattern-name) ,end-pattern-name
                                         (if multi-lines-p
                                                 (plist-get ,end-pattern-name :multi-lines)
                                             (plist-get ,end-pattern-name :single-line)))))
                     (with-current-buffer repl-buffer-name
                         (when-let* ((eat-window (get-buffer-window))
                                     (is-eat (eq termint-backend 'eat)))
                             ;; NOTE: This is crucial to ensure the
                             ;; Eat window scrolls in sync with new
                             ;; terminal output.
                             (eat--synchronize-scroll (list eat-window)))
                         (funcall send-string start-pattern)
                         (when (and multi-lines-p ,bracketed-paste-p-name)
                             (funcall send-string bracketed-paste-start))
                         (funcall send-string string)
                         (when (and multi-lines-p ,bracketed-paste-p-name)
                             (funcall send-string bracketed-paste-end))
                         (funcall send-string end-pattern))))

             (when (require 'evil nil t)
                 (evil-define-operator ,send-region-operator-name (beg end session)
                     ,(format
                       "A evil operator wrapper around `%s'. With a numeric
prefix argument, send the region to the %s process associated with
that number" send-region-func-name repl-name)
                     :move-point nil
                     (interactive "<r>P")
                     (,send-region-func-name beg end session))

                 (evil-define-operator ,source-region-operator-name (beg end session)
                     ,(format
                       "A evil operator wrapper around `%s'. With a numeric
prefix argument, send the region to the %s process associated with
that number" source-region-func-name repl-name)
                     :move-point nil
                     (interactive "<r>P")
                     (,source-region-func-name beg end session)))

             (defun ,hide-window-func-name (&optional arg)
                 ,(format
                   "hide the %s window. With numeric prefix argument, hide
the window with that number as a suffix." repl-name)
                 (interactive "P")
                 (when-let* ((eat-buffer-name
                              (if arg (format "*%s*<%d>" ,repl-name arg)
                                  (format "*%s*" ,repl-name)))
                             (buf (get-buffer eat-buffer-name))
                             (eat-buffer-window (get-buffer-window buf)))
                     (delete-window eat-buffer-window)))

             (defvar ,keymap-name
                 (let ((map (make-sparse-keymap)))
                     (define-key map "s" #',start-func-name)
                     (define-key map "r" #',send-region-func-name)
                     (define-key map "R" #',source-region-func-name)
                     (define-key map "e" #',send-string-func-name)
                     (define-key map "h" #',hide-window-func-name)
                     map)
                 ,(format "Keymap for %s REPL commands." repl-name))

             )))

(defun termint--make-tmp-file (str &optional keep-file)
    "Create a temporary file with STR.
Delete the temp file afterwards unless KEEP-FILE is non-nil."
    ;; disable output to message buffer and minibuffer.
    (let ((inhibit-message t)
          (message-log-max nil)
          file)
        (setq file (make-temp-file "" nil "_termint" str))
        (unless keep-file (run-with-idle-timer 5 nil #'delete-file file))
        file))


(defun termint--python-source-func (str)
    "Create a temporary file with STR and return a Python command to execute it."
    (let ((file (termint--make-tmp-file str t)))
        ;; Use 'compile' to ensure proper debugging context when using
        ;; PDB's `list` command
        (format "exec(compile(open(\"%s\", \"r\").read(), \"%s\", \"exec\"))"
                file file)))

(defun termint--ipython-source-func (str)
    "Create a temporary file with STR and return a iPython command to execute it."
    (let ((file (termint--make-tmp-file str t)))
        ;; The `-i` flag ensures the current environment is inherited
        ;; when executing the file
        (format "%%run -i \"%s\"" file)))

(defun termint--R-source-func (str)
    "Create a temporary file with STR and return an R command to source it."
    (let ((file (termint--make-tmp-file str)))
        (format "eval(parse(text = readr::read_file(\"%s\")))" file)))

(defun termint--bash-source-func (str)
    "Create a temporary file with STR and return a Bash command to source it."
    (let ((file (termint--make-tmp-file str)))
        (format "source %s" file)))

(defun termint--aichat-source-func (str)
    "Create a temporary file with STR and return a aichat command to source it."
    (let ((file (termint--make-tmp-file str)))
        (format ".file \"%s\"" file)))

(provide 'termint)
;;; termint.el ends here
