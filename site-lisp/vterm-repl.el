;;; vterm-repl.el --- Create and manage REPL sessions using vterm -*- lexical-binding: t; -*-

;; Author: Milan Glacier <dev@milanglacier.com>
;; Maintainer: Milan Glacier <dev@milanglacier.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29") (vterm "0.0"))

;;; Commentary:
;; This package provides a set of macros and functions to create and
;; manage REPL sessions using vterm in Emacs. It allows for creating
;; custom REPL schemas with functionalities such as starting, sending
;; code, and hiding REPL windows. This is useful for integrating
;; terminal-based REPLs with Emacs efficiently.

;;; Code:
(require 'vterm)

;;;###autoload
(defmacro vtr-create-schema (repl-name repl-cmd &rest args)
    "create a REPL schema.

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
setting the generated
variable`vtr*REPL-NAME-start-pattern'. Additionally, the value can be
a plist with two attributes: `:single-line' for specifying the string
in single-line scenarios.`:multi-lines' for defining the string in
multi-line contexts.

:end-pattern the last string to send to the REPL after sending the
region. The default is '\\r'.  You can change the behavior at run time
by setting the generated variable
`vtr*REPL-NAME-end-pattern'. Additionally the value can be a plist
with two attributes: `:single-line' for specifying the string in
single-line scenarios, and `:multi-lines' for defining the string in
multi-line contexts.

:str-process-func the function to process the string before sending it
to the REPL.  The default is `identity'. You can change the behavior
at run time by setting the generated variable
`vtr*REPL-NAME-str-process-func'.

:source-func the function to source the code content to the REPL. A
common approach involves writing the input string to a temporary file,
then returning a string that sources this file. The exact \"sourcing\"
syntax depends on the target programming language."

    (let ((start-func-name (intern (concat "vtr~" repl-name "-start")))
          (send-region-func-name (intern (concat "vtr~" repl-name "-send-region")))
          (send-region-operator-name (intern (concat "vtr~" repl-name "-send-region-operator")))
          (source-region-func-name (intern (concat "vtr~" repl-name "-source-region")))
          (source-region-operator-name (intern (concat "vtr~" repl-name "-source-region-operator")))
          (send-string-func-name (intern (concat "vtr~" repl-name "-send-string")))
          (hide-window-func-name (intern (concat "vtr~" repl-name "-hide-window")))
          (bracketed-paste-p (plist-get args :bracketed-paste-p))
          (start-pattern (or (plist-get args :start-pattern) ""))
          (end-pattern (or (plist-get args :end-pattern) "\r"))
          (str-process-func (or (plist-get args :str-process-func) ''identity))
          (source-func (or (plist-get args :source-func) ''identity))
          (repl-cmd-name (intern (concat "vtr*" repl-name "-cmd")))
          (str-process-func-name (intern (concat "vtr*" repl-name "-str-process-func")))
          (source-func-name (intern (concat "vtr*" repl-name "-source-func")))
          (bracketed-paste-p-name (intern (concat "vtr*" repl-name "-use-bracketed-paste-mode")))
          (start-pattern-name (intern (concat "vtr*" repl-name "-start-pattern")))
          (end-pattern-name (intern (concat "vtr*" repl-name "-end-pattern"))))

        `(progn

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
the buffer selected (or created). With a numeric prefix arg,create or
switch to the session with that number as a suffix."
                   repl-name repl-name)
                 (interactive "P")
                 (let ((vterm-buffer-name (format "*%s*" ,repl-name))
                       (vterm-shell
                        (if (functionp ,repl-cmd-name)
                                (funcall ,repl-cmd-name)
                            ,repl-cmd-name))
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
                        (multi-lines-p (string-match-p "\n" string))
                        (start-pattern (if (stringp ,start-pattern-name) ,start-pattern-name
                                           (if multi-line-p
                                                   (plist-get ,start-pattern-name :multi-lines)
                                               (plist-get ,start-pattern-name :single-line))))
                        (end-pattern (if (stringp ,end-pattern-name) ,end-pattern-name
                                         (if multi-lines-p
                                                 (plist-get ,end-pattern-name :multi-lines)
                                             (plist-get ,end-pattern-name :single-line)))))
                     (with-current-buffer repl-buffer-name
                         (vterm-send-string start-pattern)
                         (vterm-send-string string
                                            (if multi-lines-p
                                                    ,bracketed-paste-p-name
                                                nil))
                         (vterm-send-string end-pattern))))

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
                 (,source-region-func-name beg end session))

             (defun ,hide-window-func-name (&optional arg)
                 ,(format
                   "hide the %s window. With numeric prefix argument, hide
the window with that number as a suffix." repl-name)
                 (interactive "P")
                 (when-let* ((vterm-buffer-name
                              (if arg (format "*%s*<%d>" ,repl-name arg)
                                  (format "*%s*" ,repl-name)))
                             (buf (get-buffer vterm-buffer-name))
                             (vterm-buffer-window (get-buffer-window buf)))
                     (delete-window vterm-buffer-window)))

             )))

(defun vtr--make-tmp-file (str)
    "Create a temporary file with STR."
    ;; disable output to message buffer and minibuffer.
    (let ((inhibit-message t)
          (message-log-max nil)
          file)
        (setq file(make-temp-file "" nil "" str))
        (run-with-idle-timer 1 nil #'delete-file file)
        file))

(defun vtr--python-source-func (str)
    "Create a temporary file with STR and return a Python command to execute it."
    (let ((file (vtr--make-tmp-file str)))
        (format "exec(open(\"%s\", \"r\").read())" file)))

(defun vtr--R-source-func (str)
    "Create a temporary file with STR and return an R command to source it."
    (let ((file (vtr--make-tmp-file str)))
        (format "eval(parse(text = readr::read_file(\"%s\")))" file)))

(defun vtr--bash-source-func (str)
    "Create a temporary file with STR and return a Bash command to source it."
    (let ((file (vtr--make-tmp-file str)))
        (format "source %s" file)))

(defun vtr--aichat-source-func (str)
    "Create a temporary file with STR and return a Bash command to source it."
    (let ((file (vtr--make-tmp-file str)))
        (format ".file \"%s\"" file)))

;;;###autoload (autoload #'vtr~aichat-start "vterm-repl" nil t)
(vtr-create-schema "aichat" "aichat -s" :bracketed-paste-p t
                   :source-func #'vtr--aichat-source-func)

;;;###autoload (autoload #'vtr~ipython-start "vterm-repl" nil t)
(vtr-create-schema "ipython" "ipython" :bracketed-paste-p t
                   :source-func #'vtr--python-source-func)

;;;###autoload (autoload #'vtr~radian-start "vterm-repl" nil t)
(vtr-create-schema "radian" "radian" :bracketed-paste-p t
                   :end-pattern '(:single-line "\n" :multi-lines "")
                   :source-func #'vtr--R-source-func)

(provide 'vterm-repl)
;;; vterm-repl.el ends here
