;;; my-misc-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/vterm ()
    "open vterm at project root, if no root is found, open at the default-directory"
    (interactive)
    (require 'consult)
    (let ((default-directory (or (consult--project-root)
                                 default-directory)))
        (call-interactively #'vterm)))

;;;###autoload
(defun my/vterm-setup ()
    (setq-local confirm-kill-processes nil
                hscroll-margin 0)

    (setq vterm-max-scrollback 5000))

(provide 'my-misc-autoloads)
;;; my-misc-autoloads.el ends here
