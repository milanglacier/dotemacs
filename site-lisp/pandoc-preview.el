;;; pandoc-preview.el --- Preview markup files with pandoc -*- lexical-binding: t; -*-

(require 'subr-x)

(defvar mg-pandoc-preview-command "pandoc"
    "The pandoc executable used by `mg-pandoc-preview-buffer'.")

(defvar mg-pandoc-preview-args nil
    "Additional arguments passed to pandoc.")

(defvar mg-pandoc-preview-template-file
    (file-name-concat user-emacs-directory "assets" "pandoc-preview-template.html")
    "HTML template used by `mg-pandoc-preview-buffer'.")

(defvar mg-pandoc-preview-enabled-modes '(markdown-mode org-mode)
    "Major modes supported by `mg-pandoc-preview-buffer'.")

(defun mg-pandoc-preview--supported-mode-p ()
    (apply #'derived-mode-p mg-pandoc-preview-enabled-modes))

(defun mg-pandoc-preview--temp-file (filename)
    (file-name-concat
     (file-name-directory filename)
     (concat (file-name-base filename) ".tmp.html")))

(defun mg-pandoc-preview--build-command (filename temp-file)
    (append
     (list mg-pandoc-preview-command
           "--to=html5"
           "--standalone"
           "--template"
           mg-pandoc-preview-template-file)
     mg-pandoc-preview-args
     (list filename "-o" temp-file)))

(defun mg-pandoc-preview--delete-file-maybe (filename)
    (when (file-exists-p filename)
        (delete-file filename)))

(defun mg-pandoc-preview--stderr-text (stderr-buffer)
    (when-let* ((stderr-text
                 (string-trim
                  (with-current-buffer stderr-buffer
                      (buffer-string))))
                ((not (string-empty-p stderr-text))))
        stderr-text))

(defun mg-pandoc-preview--handle-success (temp-file stderr-buffer)
    (browse-url-of-file temp-file)
    (run-with-idle-timer
     3 nil
     #'mg-pandoc-preview--delete-file-maybe
     temp-file)
    (kill-buffer stderr-buffer))

(defun mg-pandoc-preview--handle-failure (stderr-buffer)
    (message "pandoc failed to convert the file to html")
    (when-let* ((stderr-text
                 (mg-pandoc-preview--stderr-text stderr-buffer)))
        (message "%s" stderr-text)
        (display-buffer stderr-buffer)))

(defun mg-pandoc-preview--process-sentinel (process temp-file stderr-buffer)
    (when (memq (process-status process) '(exit signal))
        (if (= (process-exit-status process) 0)
                (mg-pandoc-preview--handle-success temp-file stderr-buffer)
            (mg-pandoc-preview--handle-failure stderr-buffer))))

;;;###autoload
(defun mg-pandoc-preview-buffer ()
    "Preview the current file as HTML using pandoc."
    (interactive)
    (let ((filename (buffer-file-name)))
        (cond
         ((not (mg-pandoc-preview--supported-mode-p))
          (user-error "Unsupported filetype"))
         ((not (and filename (file-readable-p filename)))
          (user-error "Not on a file buffer"))
         ((not (executable-find mg-pandoc-preview-command))
          (user-error "Pandoc is not installed"))
         ((not (file-readable-p mg-pandoc-preview-template-file))
          (user-error "Pandoc preview template is missing: %s"
                      mg-pandoc-preview-template-file))
         (t
          (let* ((temp-file (mg-pandoc-preview--temp-file filename))
                 (stderr-buffer (get-buffer-create "*pandoc-preview stderr*")))
              (with-current-buffer stderr-buffer
                  (erase-buffer))
              (make-process
               :name "pandoc-preview"
               :command (mg-pandoc-preview--build-command filename temp-file)
               :buffer stderr-buffer
               :stderr stderr-buffer
               :noquery t
               :sentinel
               (lambda (process _event)
                   (mg-pandoc-preview--process-sentinel
                    process temp-file stderr-buffer))))))))

(provide 'pandoc-preview)
;;; pandoc-preview.el ends here
