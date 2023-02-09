;;; my-basics-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/display-truncation-and-wrap-indicator-as-whitespace ()
    (when (not (char-table-p buffer-display-table))
        (setq buffer-display-table (make-display-table)))
    (set-display-table-slot buffer-display-table 'truncation 32)
    (set-display-table-slot buffer-display-table 'wrap 32))

(provide 'my-basics-autoloads)
;;; my-basics-autoloads ends here
