;;; my-vcs-autoloads.el.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/project-todos ()
    "Find `hl-todo--regex' items in project using `consult-ripgrep'"
    (interactive)
    (require 'hl-todo)
    (consult-ripgrep nil hl-todo--regexp))

;;;###autoload (autoload #'my~project-magit "my-vcs-autoloads" nil t)
(defalias #'my~project-magit (wrap-command-at-project-root #'magit)
    "open `magit' at project root.")

(provide 'my-vcs-autoloads.el)
;;; my-vcs-autoloads.el.el ends here
