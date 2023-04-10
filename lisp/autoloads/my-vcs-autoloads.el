;;; my-vcs-autoloads.el.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/project-todos ()
    "Find `hl-todo--regex' items in project using `consult-ripgrep'"
    (interactive)
    (require 'hl-todo)
    (consult-ripgrep nil hl-todo--regexp))

;;;###autoload
(defun my~project-magit ()
    "Open `magit' at current project."
    (interactive)
    (let ((default-directory (my/project-root-or-default-dir)))
        (call-interactively #'magit)))

(provide 'my-vcs-autoloads.el)
;;; my-vcs-autoloads.el.el ends here
