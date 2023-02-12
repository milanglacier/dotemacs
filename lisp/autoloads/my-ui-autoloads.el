;;; my-ui-autoloads.el -*- lexical-binding: t; -*-

(defun my/display-org-agenda-list ()
    "if current window is scratch buffer, then replace this buffer
with org agenda otherwise open org-agenda with the specified way
(i.e create a new tab)"
    (when (equal (buffer-name) "*scratch*")
        (let ((display-buffer-alist
               '(("Org Agenda"
                  (display-buffer-same-window)))))
            (call-interactively #'org-agenda-list))))

;;;###autoload
(defun my/delayed-startup-screen ()
    "`org-agenda-list' is slow, don't run it immediately after startup"
    (run-with-idle-timer 2 nil #'my/display-org-agenda-list))

;;;###autoload
(defmacro my/tab-bar-go-to-tab-macro (number)
    (let ((fun (intern (format "my/tab-bar-go-to-tab-%d" number))))
        `(defun ,fun ()
             ,(format "go to tab %d" number)
             (interactive)
             (tab-bar-select-tab ,number))))

;;;###autoload
(defun my/set-scratch-directory (old-fun &rest args)
    "After creating a new tab, the default buffer to be displayed is
scratch buffer whose directory is set to where emacs is initialized.
Change it to the directory of previous buffer where `tab-bar-new-tab'
is called."
    (let ((current-dir default-directory))
        (apply old-fun args)
        (setq-local default-directory current-dir)))

(provide 'my-ui-autoloads)
;;; my-ui-autoloads ends here
