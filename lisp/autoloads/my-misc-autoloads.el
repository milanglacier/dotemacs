;;; my-misc-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/vterm ()
    "open vterm at project root, if no root is found, open at the default-directory"
    (interactive)
    (let ((default-directory (my/project-root-or-default-dir)))
        (call-interactively #'vterm)))

;;;###autoload
(defun my/project-root-or-default-dir ()
    "If a project root is found, return it. Otherwise return `default-directory'."
    (if-let ((proj (project-current)))
            (project-root proj)
        default-directory))

;;;###autoload
(defun my/ibuffer-vc-setup ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))

(defun my:dired-subtree-add-nerd-icons ()
    (interactive)
    (revert-buffer))

;;;###autoload
(defun my:dired-subtree-toggle-nerd-icons ()
    (when (require 'dired-subtree nil t)
        (if nerd-icons-dired-mode
                (advice-add #'dired-subtree-toggle :after #'my:dired-subtree-add-nerd-icons)
            (advice-remove #'dired-subtree-toggle #'my:dired-subtree-add-nerd-icons))))

(provide 'my-misc-autoloads)
;;; my-misc-autoloads.el ends here
