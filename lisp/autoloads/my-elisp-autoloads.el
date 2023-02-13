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

(provide 'my-elisp-autoloads)
;; my-elisp-autoloads.el ends here
