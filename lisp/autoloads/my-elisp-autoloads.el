;;; my-elisp-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/helpful-display-buffer (buf)
    "If a helpful buffer window is already opened, should use it,
don't occupy other window. Make sure it is a side window, such that
when you press q and want to close the help window), this window will
be completely removed, i.e. the window won't be displayed showing
other buffer."
    (pop-to-buffer buf `((display-buffer-reuse-mode-window display-buffer-in-side-window)
                         (window-height . 0.5)
                         (window-width . 0.5)
                         ;; if there are multiple side window
                         ;; prefer the helpful window to the relatively left position
                         (slot . ,(alist-get 'helpful my/side-window-slots)))))

;;;###autoload
(defun my/helpful-lookup-symbl-at-point ()
    (interactive)
    (helpful-symbol (symbol-at-point)))

;;;###autoload
(defun my/elisp-loop-up-symbol (beg end)
    "Look up for the symbol under point, if region is active, use
        the selected region as the symbol" (interactive "r")
    (if (use-region-p)
            (helpful-symbol (intern (buffer-substring beg end)))
        (helpful-symbol (symbol-at-point))))

(defun my/emacs-lisp-outline-level ()
    "Return outline level for comment at point.
Intended to replace `lisp-outline-level'."
    (- (match-end 1) (match-beginning 1)))

;;;###autoload
(defun my/elisp-setup ()
    ;; referenced from doomemacs

    (setq-local outline-regexp "[ \t]*;;;\\(;*\\**\\) [^ \t\n]"
                outline-level #'my/emacs-lisp-outline-level)
    (outline-minor-mode)
    (highlight-quoted-mode))

;;;copied from https://github.com/Fuco1/.emacs.d/blob/master/site-lisp/my-redef.el
;;;###autoload
(defun my/lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (cond
         ;; car of form doesn't seem to be a symbol, or is a keyword
         ((and (elt state 2)
               (or (not (looking-at "\\sw\\|\\s_"))
                   (looking-at ":")))
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
                  (progn (goto-char calculate-lisp-indent-last-sexp)
                         (beginning-of-line)
                         (parse-partial-sexp (point)
                                             calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
         ((and (save-excursion
                   (goto-char indent-point)
                   (skip-syntax-forward " ")
                   (not (looking-at ":")))
               (save-excursion
                   (goto-char orig-point)
                   (looking-at ":")))
          (save-excursion
              (goto-char (+ 2 (elt state 1)))
              (current-column)))
         (t
          (let ((function (buffer-substring (point)
                                            (progn (forward-sexp 1) (point))))
                method)
              (setq method (or (function-get (intern-soft function)
                                             'lisp-indent-function)
                               (get (intern-soft function) 'lisp-indent-hook)))
              (cond ((or (eq method 'defun)
                         (and (null method)
                              (> (length function) 3)
                              (string-match "\\`def" function)))
                     (lisp-indent-defform state indent-point))
                    ((integerp method)
                     (lisp-indent-specform method state
                                           indent-point normal-indent))
                    (method
                     (funcall method indent-point state))))))))

(provide 'my-elisp-autoloads)
;; my-elisp-autoloads.el ends here
