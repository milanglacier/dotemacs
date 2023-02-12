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

(defvar my/eldoc-buffer-dwim-key "K"
    "The key to enable dwim behavior on displaying eldoc buffer")

;;;###autoload
(defun my/eldoc-buffer-dwim ()
    "When eldoc buffer window is not opened, display the eldoc
window. Pressing `my/eldoc-buffer-dwim-key' again within a short
period (1s currently as hard coded) will move your focus on the eldoc
window. If the shorter period has gone, calling this command will
close the eldoc window. Currently this dwim hack is only effective in
`eglot-mode-map' as it is hardcoded."
    (interactive)
    (if-let ((eldoc-win (get-buffer-window "*eldoc*")))
            (delete-window eldoc-win)
        (progn
            (eldoc-doc-buffer)
            (my/eldoc-dwim-hack))))

(defun my/eldoc-dwim-hack ()
    "bind `my/eldoc-buffer-dwim-key' locally to a command that
will switch to the eldoc buffer, and unbind the key after a short
period (1s as hard coded.)"
    (general-define-key
     :keymaps 'eglot-mode-map
     :states '(normal motion)
     my/eldoc-buffer-dwim-key #'my/eldoc-focus)

    (run-with-idle-timer 1 nil #'my/eldoc-locally-unbind))

(defun my/eldoc-locally-unbind ()
    (general-define-key
     :states '(normal motion)
     :keymaps 'eglot-mode-map
     my/eldoc-buffer-dwim-key #'my/eldoc-buffer-dwim))

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
