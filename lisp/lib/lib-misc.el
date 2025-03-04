;;; lib-misc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun call-command-at-project-root (orig-fun &rest args)
    "call command at project root, if no root is found, open at the default-directory"
    (let ((default-directory (mg-project-root-or-default-dir)))
        (apply orig-fun args)))

;;;###autoload
(defun wrap-command-at-project-root (orig-cmd)
    "create a new command that calls orig-cmd at project root, if no root is found, open at the default-directory"
    (lambda ()
        (interactive)
        (let ((default-directory (mg-project-root-or-default-dir)))
            (call-interactively orig-cmd))))

;;;###autoload
(defun mg-project-root-or-default-dir ()
    "If a project root is found, return it. Otherwise return `default-directory'."
    (if-let* ((proj (project-current)))
            (project-root proj)
        default-directory))

;;;###autoload
(defun mg-dired-find-file-other-tab ()
    "In Dired, visit this file or directory in another window."
    (interactive)
    (dired--find-file #'find-file-other-tab (dired-get-file-for-visit)))

;;;###autoload
(defun mg-ibuffer-vc-setup ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))

(defun mg--dired-subtree-add-nerd-icons ()
    (interactive)
    (revert-buffer))

;;;###autoload
(defun mg--dired-subtree-toggle-nerd-icons ()
    (when (require 'dired-subtree nil t)
        (if nerd-icons-dired-mode
                (advice-add #'dired-subtree-toggle :after #'mg--dired-subtree-add-nerd-icons)
            (advice-remove #'dired-subtree-toggle #'mg--dired-subtree-add-nerd-icons))))

(provide 'lib-misc)
;;; lib-misc.el ends here
