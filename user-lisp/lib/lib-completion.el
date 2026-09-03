;;; lib-completion.el -*- lexical-binding: t; -*-

;;; Cape

(defalias #'mg-eglot-citre-capf
    (cape-capf-super #'eglot-completion-at-point #'citre-completion-at-point))

(defalias #'mg-yas-eglot-citre-capf
    ;; Citre can return the same completion text for tags at different
    ;; locations or duplicate an Eglot candidate.  Candidate metadata is
    ;; stored in text properties, which `delete-dups' ignores.  It keeps
    ;; the first match, preserving Yasnippet -> Eglot -> Citre priority.
    (cape-capf-sort
     (cape-capf-super #'yasnippet-capf
                      #'eglot-completion-at-point
                      #'citre-completion-at-point)
     #'delete-dups))

;;;###autoload
(defun mg-toggle-citre-eglot-capf ()
    (if (eglot-managed-p)
            (add-to-list 'completion-at-point-functions #'mg-eglot-citre-capf)
        (setq-local completion-at-point-functions
                    (delq #'mg-eglot-citre-capf completion-at-point-functions))))

;;;###autoload
(defun mg-toggle-yas-eglot-citre-capf ()
    (if (eglot-managed-p)
            (add-to-list 'completion-at-point-functions #'mg-yas-eglot-citre-capf)
        (setq-local completion-at-point-functions
                    (delq #'mg-yas-eglot-citre-capf completion-at-point-functions))))

;;;###autoload
(defun mg-complete-ctags ()
    "Complete ctags completions using the minibuffer."
    (interactive)
    (cape-interactive #'citre-completion-at-point))

;;;###autoload
(defun mg-cape-dabbrev-buffers ()
    "Return buffers with the same major mode that are smaller than 1MB.
Larger buffers are skipped to keep completion responsive."
    (seq-filter (lambda (buf) (< (buffer-size buf) (* 1024 1024)))
                (cape-same-mode-buffers)))

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

(provide 'lib-completion)
;;; lib-completion.el ends here
