;;; lib-completion.el -*- lexical-binding: t; -*-

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

(defalias #'mg-complete-ctags (cape-capf-interactive #'citre-completion-at-point)
    "complete ctags completions using minibuffer")

(provide 'lib-completion)
;;; lib-completion ends here
