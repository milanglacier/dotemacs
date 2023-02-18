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

;; DEPRECATED Remove when 28 support is dropped.
(unless (fboundp 'lisp--local-defform-body-p)
    (fset 'lisp--local-defform-body-p #'ignore))

;;;copied from doomemacs
;;;###autoload
(defun my/lisp-indent-function (indent-point state)
    "A replacement for `lisp-indent-function'.

Indents plists more sensibly. Adapted from
https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned"
    (let ((normal-indent (current-column))
          (orig-point (point))
          ;; TODO Refactor `target' usage (ew!)
          target)
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (cond ((and (elt state 2)
                    (or (eq (char-after) ?:)
                        (not (looking-at-p "\\sw\\|\\s_"))))
               (if (lisp--local-defform-body-p state)
                       (lisp-indent-defform state indent-point)
                   (unless (> (save-excursion (forward-line 1) (point))
                              calculate-lisp-indent-last-sexp)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (beginning-of-line)
                       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
                   (backward-prefix-chars)
                   (current-column)))
              ((and (save-excursion
                        (goto-char indent-point)
                        (skip-syntax-forward " ")
                        (not (eq (char-after) ?:)))
                    (save-excursion
                        (goto-char orig-point)
                        (and (eq (char-after) ?:)
                             (eq (char-before) ?\()
                             (setq target (current-column)))))
               (save-excursion
                   (move-to-column target t)
                   target))
              ((let* ((function (buffer-substring (point) (progn (forward-sexp 1) (point))))
                      (method (or (function-get (intern-soft function) 'lisp-indent-function)
                                  (get (intern-soft function) 'lisp-indent-hook))))
                   (cond ((or (eq method 'defun)
                              (and (null method)
                                   (> (length function) 3)
                                   (string-match-p "\\`def" function)))
                          (lisp-indent-defform state indent-point))
                         ((integerp method)
                          (lisp-indent-specform method state indent-point normal-indent))
                         (method
                          (funcall method indent-point state))))))))

(provide 'my-elisp-autoloads)
;; my-elisp-autoloads.el ends here
