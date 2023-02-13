;;; my-langtools-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/do-not-use-citre-imenu ()
    (setq-local citre-enable-imenu-integration nil))

;;;###autoload
(defun my/do-not-use-citre-xref ()
    (setq-local citre-enable-xref-integration nil))

;;;###autoload
(defun my/do-not-use-citre-capf ()
    (setq-local citre-enable-capf-integration nil))

(defalias #'my/eglot-citre-capf
    (cape-super-capf #'eglot-completion-at-point #'citre-completion-at-point))

;;;###autoload
(defun my/toggle-citre-eglot-capf ()
    (if (eglot-managed-p)
            (add-to-list 'completion-at-point-functions #'my/eglot-citre-capf)
        (setq-local completion-at-point-functions
                    (delq #'my/eglot-citre-capf completion-at-point-functions))))

;;;###autoload
(defun my/eglot-do-not-use-imenu ()
    (setq-local eglot-stay-out-of `("imenu" ,@eglot-stay-out-of)))

(defun my/eldoc-buffer-dwim-fallback ()
    "When eldoc buffer window is not opened, display the eldoc
window. Pressing `my/eldoc-buffer-dwim-key' again within a short
period (1s currently as hard coded) will move your focus on the eldoc
window. If the shorter period has gone, calling this command will
close the eldoc window."
    (interactive)
    (if-let ((eldoc-win (get-buffer-window "*eldoc*")))
            (delete-window eldoc-win)
        (progn
            (eldoc-doc-buffer)
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
                 (funcall ',func)))))

(provide 'my-langtools-autoloads)
;;; my-langtools-autoloads.el ends here
