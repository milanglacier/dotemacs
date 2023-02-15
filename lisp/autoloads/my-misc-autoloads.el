;;; my-misc-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/vterm ()
    "open vterm at project root, if no root is found, open at the default-directory"
    (interactive)
    (require 'consult)
    (let ((default-directory (my/project-root-or-default-dir)))
        (call-interactively #'vterm)))

;;;###autoload
(defun my/project-root-or-default-dir ()
    "If a project root is found, return it. Otherwise return `default-directory'."
    (if-let ((proj (project-current)))
            (project-root proj)
        default-directory))

;;;###autoload
(defun my/vterm-setup ()
    (setq-local confirm-kill-processes nil
                hscroll-margin 0)

    (setq vterm-max-scrollback 5000))

;;;###autoload
(defun my/ibuffer-vc-setup ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))

(provide 'my-misc-autoloads)
;;; my-misc-autoloads.el ends here
