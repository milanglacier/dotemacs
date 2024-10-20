;;; lib-vcs.el.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mg-project-todos ()
    "Find `hl-todo--regex' items in project using `consult-ripgrep'"
    (interactive)
    (require 'hl-todo)
    (consult-ripgrep nil hl-todo--regexp))

;;;###autoload (autoload #'my-project-magit "lib-vcs" nil t)
(defalias #'my-project-magit (wrap-command-at-project-root #'magit)
    "open `magit' at project root.")

(provide 'lib-vcs.el)
;;; lib-vcs.el.el ends here
