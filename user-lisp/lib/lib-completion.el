;;; lib-completion.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mg-complete-ctags ()
    "Complete ctags completions using the minibuffer."
    (interactive)
    (cape-interactive #'citre-completion-at-point))

;;; Company

;;;###autoload
(defun mg-company-abort ()
    (when company-candidates
        (company-abort)))

;; We follow a suggestion by company maintainer u/hvis:
;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
;;;###autoload
(defun mg-company-completion-styles (capf-fn &rest args)
    (let ((completion-styles '(basic partial-completion emacs22)))
        (apply capf-fn args)))

;;; Corfu

;;;###autoload
(defun mg-cape-dabbrev-buffers ()
    "Return buffers with the same major mode that are smaller than 1MB.
Larger buffers are skipped to keep completion responsive."
    (seq-filter (lambda (buf) (< (buffer-size buf) (* 1024 1024)))
                (cape-same-mode-buffers)))

(provide 'lib-completion)
;;; lib-completion.el ends here
