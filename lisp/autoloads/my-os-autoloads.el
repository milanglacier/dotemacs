;;; my-os-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/macos-cmd-w ()
    "If there is only one tab, close emacs, otherwise close one tab"
    (interactive)
    (if (> (length (tab-bar-tabs)) 1)
            (tab-bar-close-tab)
        (evil-quit-all)))

(provide 'my-os-autoloads)
;;; my-os-autoloads.el ends here
