;;; my-langtools-autoloads.el -*- lexical-binding: t; -*-

(defalias #'my/eglot-citre-capf
    (cape-super-capf #'eglot-completion-at-point #'citre-completion-at-point))

;;;###autoload
(defun my/toggle-citre-eglot-capf ()
    (if (eglot-managed-p)
            (add-to-list 'completion-at-point-functions #'my/eglot-citre-capf)
        (setq-local completion-at-point-functions
                    (delq #'my/eglot-citre-capf completion-at-point-functions))))

(defun my/eldoc-buffer-dwim-fallback ()
    "When eldoc buffer window is not opened, display the eldoc
window. Pressing \\[my/eldoc-buffer-dwim] again within a short
period (1s currently as hard coded) will move your focus on the eldoc
window. If the shorter period has gone, calling this command will
close the eldoc window."
    (interactive)
    (if-let ((eldoc-win (get-buffer-window "*eldoc*")))
            (delete-window eldoc-win)
        (progn
            (eldoc-doc-buffer t)
            (my/eldoc-dwim-hack)))
    )

;;;###autoload (autoload #'my/eldoc-buffer-dwim "my-langtools-autoloads" nil t)
(defalias #'my/eldoc-buffer-dwim #'my/eldoc-buffer-dwim-fallback)

(defun my/eldoc-dwim-hack ()
    "Alias `my/eldoc-buffer-dwim' to `my/eldoc-focus'. And after the
period, revert the action."
    (defalias #'my/eldoc-buffer-dwim #'my/eldoc-focus)
    (run-with-idle-timer 1 nil #'my/eldoc-dwim-revert))

(defun my/eldoc-dwim-revert ()
    (defalias #'my/eldoc-buffer-dwim #'my/eldoc-buffer-dwim-fallback))

(defun my/eldoc-focus ()
    "focus on the eldoc window"
    (interactive)
    (when (get-buffer-window "*eldoc*")
        (select-window (get-buffer-window "*eldoc*"))))

;;;###autoload
(defmacro my/xref-move-in-original-src-macro (func)
    "There can only be one xref buffer. That is, if you find
references of other symbol the previous one will be overwritten. The
official `xref-next-line' `xref-next-group' only allows you to move
the location in the src buffer when your point is in the xref buffer
window. This macro creates funcs that allow you to move current window
to next xref location."
    (let ((xref-move-func (intern (format "my/%s" func)))
          (xref-move-func-desc (format "Effectively calling %s in the src window." func)))
        `(defun ,xref-move-func ()
             ,xref-move-func-desc
             (interactive)
             (with-current-buffer "*xref*"
                 (funcall #',func)))))

;; TODO: Utilize the contextual information from previous code blocks
;; PLAN:
;; 1. Get the contents of previous code blocks
;; 2. Paste the code blocks into the temp buffer
;; 3. When finish editing, delete those inserted contextual code.

;;;###autoload
(defun my/markdown-src-lsp-setup()
    "eglot requires the buffer to be a file to be able to attach to
the lsp. Thus the indirect buffer created by `edit-indirect' needs to
be associated with a real file."
    (setq-local buffer-file-name (file-name-concat default-directory "markdown-src.tmp"))
    (eglot-ensure))

;;; copied from Centaur Emacs
;;;###autoload
(defmacro my/org-babel-lsp-setup (lang)
    "Support LANG in org source code block."
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (my-setup (intern (format "my/lsp-setup-for--%s" (symbol-name edit-pre)))))
        `(progn
             (defun ,my-setup (info)
                 (setq buffer-file-name
                       (or (->> info caddr (alist-get :file))
                           (file-name-concat default-directory "org-babel-src.tmp")))
                 (eglot-ensure))

             (if (fboundp #',edit-pre)
                     (advice-add #',edit-pre :after #',my-setup)
                 (progn
                     (defun ,edit-pre (info)
                         (,my-setup info)))))))

(provide 'my-langtools-autoloads)
;;; my-langtools-autoloads.el ends here
