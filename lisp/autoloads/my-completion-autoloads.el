;;; my-completion-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/company-abort ()
    (when company-candidates
        (company-abort)))

;; We follow a suggestion by company maintainer u/hvis:
;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
;;;###autoload
(defun my/company-completion-styles (capf-fn &rest args)
    (let ((completion-styles '(basic partial-completion emacs22)))
        (apply capf-fn args)))

(provide 'my-completion-autoloads)
;;; my-completion-autoloads ends here
