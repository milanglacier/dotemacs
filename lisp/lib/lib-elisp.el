;;; lib-elisp.el -*- lexical-binding: t; -*-

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
    (highlight-quoted-mode)

    (font-lock-add-keywords
     'emacs-lisp-mode
     '((my:emacs-lisp-highlight-vars-and-faces . my&emacs-lisp--face)))

    ;; copied from doomemacs
    (setq-local imenu-generic-expression
                `(("Evil commands" "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
                  ("Unit tests" "^\\s-*(\\(?:ert-deftest\\|describe\\) +\"\\([^\")]+\\)\"" 1)
                  ("Package" "^\\s-*\\(?:;;;###package\\|(\\(?:package!\\|use-package!?\\|after!\\)\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
                  ("Major modes" "^\\s-*(define-derived-mode +\\([^ ()\n]+\\)" 1)
                  ("Minor modes" "^\\s-*(define-\\(?:global\\(?:ized\\)?-minor\\|generic\\|minor\\)-mode +\\([^ ()\n]+\\)" 1)
                  ("Macros" "^\\s-*(\\(?:cl-\\)?def\\(?:ine-compile-macro\\|macro\\) +\\([^ )\n]+\\)" 1)
                  ("Inline functions" "\\s-*(\\(?:cl-\\)?defsubst +\\([^ )\n]+\\)" 1)
                  ("CLI Command" "^\\s-*(\\(def\\(?:cli\\|alias\\|obsolete\\|autoload\\)! +\\([^\n]+\\)\\)" 1)
                  ("Functions" "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\|-memoized!\\) +\\([^ ,)\n]+\\)" 1)
                  ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var\\(?:-local\\)?\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                  ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)))
    )

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

(defvar my&emacs-lisp--face nil)

(defun my:emacs-lisp-highlight-vars-and-faces (end)
    "Match defined variables and functions.

Functions are differentiated into special forms, built-in functions and
library/userland functions"
    (catch 'matcher
        (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
            (let ((ppss (save-excursion (syntax-ppss))))
                (cond ((nth 3 ppss)  ; strings
                       (search-forward "\"" end t))
                      ((nth 4 ppss)  ; comments
                       (forward-line +1))
                      ((let ((symbol (intern-soft (match-string-no-properties 0))))
                           (and (cond ((null symbol) nil)
                                      ((eq symbol t) nil)
                                      ((keywordp symbol) nil)
                                      ((special-variable-p symbol)
                                       (setq my&emacs-lisp--face 'font-lock-variable-name-face))
                                      ((and (fboundp symbol)
                                            (eq (char-before (match-beginning 0)) ?\()
                                            (not (memq (char-before (1- (match-beginning 0)))
                                                       (list ?\' ?\`))))
                                       (let ((unaliased (indirect-function symbol)))
                                           (unless (or (macrop unaliased)
                                                       (special-form-p unaliased))
                                               (let (unadvised)
                                                   (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                                   (setq unaliased (indirect-function unadvised)))))
                                                   unaliased)
                                               (setq my&emacs-lisp--face
                                                     (if (subrp unaliased)
                                                             'font-lock-constant-face
                                                         'font-lock-function-name-face))))))
                                (throw 'matcher t)))))))
        nil))

(provide 'lib-elisp)
;; lib-elisp.el ends here
