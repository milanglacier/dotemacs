;;; consult-citre.el -*- lexical-binding: t; -*-
;; URL `https://emacs-china.org/t/citre-ctags/17604/666'

(require 'citre)
(require 'consult)

(defun consult-citre-readtags--build-cmd
        (tagsfile &optional name match case-fold filter sorter action)
    "Build readtags command.
See `citre-readtags-get-tags' to know about NAME, MATCH, CASE-FOLD,
FILTER, and SORTER.  ACTION can be nil, to get regular tags, or
any valid actions in readtags, e.g., \"-D\", to get pseudo tags."
    (let* ((match (or match 'exact))
           (extras (concat
                    "-Ene"
                    (pcase match
                        ('exact "")
                        ('prefix "p")
                        (_ (error "Unexpected value of MATCH")))
                    (if case-fold "i" "")))
           (tagsfile (substring-no-properties tagsfile))
           (name (when name (substring-no-properties name)))
           (filter (citre-readtags--strip-text-property-in-list filter))
           (sorter (citre-readtags--strip-text-property-in-list sorter))
           inhibit-message
           cmd)
        ;; Program name
        (push (or citre-readtags-program "readtags") cmd)
        ;; Read from this tags file
        (push "-t" cmd)
        (push (file-local-name tagsfile) cmd)
        ;; Filter expression
        (when filter (push "-Q" cmd) (push (format "%S" filter) cmd))
        (when sorter (push "-S" cmd) (push (format "%S" sorter) cmd))
        ;; Extra arguments
        (push extras cmd)
        ;; Action
        (if action (push action cmd)
            (if (or (null name) (string-empty-p name))
                    (push "-l" cmd)
                (push "-" cmd)
                (push name cmd)))
        (nreverse cmd)))

(defun consult-citre-readtags--builder (input)
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended t)))
        (setq re (consult--join-regexps re 'extended))
        (cons
         (consult-citre-readtags--build-cmd
          (citre-tags-file-path)
          nil nil t
          `((string->regexp ,re :case-fold true) $name)
          nil
          nil)
         hl)
        ))

(defun consult-citre-readtags--format (lines)
    (mapcar (lambda (line)
                (let* ((tag (citre-readtags--parse-line
                             line
                             (citre-readtags-tags-file-info (citre-tags-file-path))
                             '(name input pattern line kind) '() '()
                             '(ext-abspath ext-kind-full) '() '() t
                             ))
                       (str (citre-get-tag-field 'name tag)))
                    (propertize str 'citre-tag tag))
                )
            lines))

(defun consult-citre-readtags--annotate (root str)
    (let ((tag (get-text-property 0 'citre-tag str)))
        (consult--annotate-align str (citre-make-tag-str tag nil
                                                         '(annotation)
                                                         `(location :suffix ":" :root ,root)
                                                         '(content :ensure t)))))


;;;###autoload
(defun consult-citre (initial)
    "Read a tag from minibuffer and jump to the tag."
    (interactive "P")
    (citre-goto-tag
     (consult--read
      (consult--async-command #'consult-citre-readtags--builder
                              (consult--async-transform consult-citre-readtags--format)
                              (consult--async-highlight #'consult-citre-readtags--builder))
      :prompt "Tag: "
      :keymap consult-async-map
      :annotate (apply-partially #'consult-citre-readtags--annotate (citre-project-root))
      :require-match t
      :category 'citre-tag
      :initial (consult--async-split-initial initial)
      :lookup (apply-partially #'consult--lookup-prop 'citre-tag))))

(provide 'consult-citre)
;;; consult-citre.el ends here
