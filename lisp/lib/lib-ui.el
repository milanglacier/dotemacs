;;; lib-ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mg-display-truncation-and-wrap-indicator-as-whitespace ()
    (when (not (char-table-p buffer-display-table))
        (setq buffer-display-table (make-display-table)))
    (set-display-table-slot buffer-display-table 'truncation 32)
    (set-display-table-slot buffer-display-table 'wrap 32))

;;;###autoload
(defmacro mg-tab-bar-go-to-tab-macro (number)
    (let ((fun (intern (format "mg-tab-bar-go-to-tab-%d" number))))
        `(defun ,fun ()
             ,(format "go to tab %d" number)
             (interactive)
             (tab-bar-select-tab ,number))))

;;;###autoload
(defun mg-set-scratch-directory (old-fun &rest args)
    "After creating a new tab, the default buffer to be displayed is
scratch buffer whose directory is set to where emacs is initialized.
Change it to the directory of previous buffer where `tab-bar-new-tab'
is called."
    (let ((current-dir default-directory))
        (apply old-fun args)
        (setq-local default-directory current-dir)))

(defun mg--tab-bar-new-buffer ()
    (get-buffer-create "*scratch*"))

;;;###autoload
(defun mg--font-set-small-mono-font ()
    "Set the default font to a smaller sized font for current buffer."
    (face-remap-add-relative 'default :height 140))

;;;###autoload
(defun mg--font-set-small-variable-font ()
    "Set the default font to a smaller sized font for current buffer."
    (face-remap-add-relative 'default :height 140 :family "Bookerly"))

(defvar mg-tab-bar-tab-name-open "")
(defvar mg-tab-bar-tab-name-close "")
(defvar mg-tab-bar-group-name-open " ")
(defvar mg-tab-bar-group-name-close " ")

(defun mg--tab-bar-add-custom-boundaries (name _ _)
    "Add custom separators around tab names in the tab-bar.
Unlike `tab-bar-separator' which uses identical symbols for both sides,
this function applies different symbols defined by
`mg-tab-bar-tab-name-open' and `mg-tab-bar-tab-name-close' as
boundaries."
    (concat mg-tab-bar-tab-name-open
            name
            mg-tab-bar-tab-name-close))

(defun mg--tab-bar-tab-group-format (tab i &optional current-p)
    "This is a slightly modified version of
`tab-bar-tab-group-format-default', which is the default value of
`tab-bar-tab-group-format', except that it adds two symbols indicating
the group more distinguisably."
    (propertize
     (concat mg-tab-bar-group-name-open
             (if tab-bar-tab-hints (format "%d " i) "")
             (funcall tab-bar-tab-group-function tab)
             mg-tab-bar-group-name-close)
     'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))

(defun mg--get-tab-name (buffer alist)
    "Retrieve the name of a tab associated with a BUFFER.  This
function is intended for use with `display-buffer-in-tab'.  The
behavior is straightforward: if a tab already exists with the same
name as the BUFFER, it is reused; otherwise, a new tab is created."
    (buffer-name buffer))

(provide 'lib-ui)
;;; lib-ui ends here
