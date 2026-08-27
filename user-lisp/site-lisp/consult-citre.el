;;; consult-citre.el -*- lexical-binding: t; -*-
;; author: Chingyat
;; URL `https://emacs-china.org/t/citre-ctags/17604/666'
;; URL `https://emacs-china.org/t/citre-ctags/17604/669'

(require 'citre)
(require 'consult)
(require 'consult-xref)
(require 'embark)

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
         (append (consult-citre-readtags--build-cmd
                  (citre-tags-file-path)
                  nil nil t
                  `((string->regexp ,re :case-fold true) $name)
                  nil
                  (car-safe opts))
                 (cdr-safe opts))
         hl)
        ))

(defun consult-citre-readtags--format (info lines)
    (mapcar (lambda (line)
                (let* ((tag (citre-readtags--parse-line
                             line
                             info
                             '(name input pattern line kind) '() '()
                             '(ext-abspath ext-kind-full) '() '() t))
                       (group (citre-get-tag-field 'ext-abspath tag))
                       (line (citre-get-tag-field 'line tag))
                       (cand (consult--format-file-line-match
                              group
                              line
                              (citre-make-tag-str tag nil
                                                  '(annotation :prefix "(" :suffix ")"
                                                               ;; In xref buffer, we may want to jump to
                                                               ;; the tags with these anonymous names.
                                                               :full-anonymous-name t)
                                                  '(content :ensure t)))))
                    (add-text-properties 0 (length cand) `(consult-citre-tag ,tag consult--prefix-group ,group) cand)
                    cand))
            lines))

;;;###autoload
(defun consult-citre (initial)
    "Read a tag from minibuffer and jump to the tag."
    (interactive "P")
    (let ((info (citre-readtags-tags-file-info (citre-tags-file-path))))
        (xref-pop-to-location
         (consult--read
          (consult--process-collection
                  #'consult-citre-readtags--builder
              :transform (consult--async-transform (lambda (lines) (consult-citre-readtags--format info lines)))
              :highlight #'consult-citre-readtags--builder)
          :prompt "Tag: "
          :keymap consult-async-map
          :require-match t
          :category 'consult-citre
          :initial initial
          :group #'consult--prefix-group
          :state (consult-xref--preview #'switch-to-buffer)
          :lookup (lambda (&rest args)
                      (when-let* ((tag (apply #'consult--lookup-prop 'consult-citre-tag args)))
                          (citre-xref--make-object tag)))))))

(defun consult-citre--embark-export-xref (items)
    "Create an xref buffer listing ITEMS."
    (let ((xrefs))
        (dolist-with-progress-reporter (item items)
                "Exporting Xrefs..."
            (redisplay)
            (push  (citre-xref--make-object (get-text-property 0 'consult-citre-tag item))
                   xrefs))
        (set-buffer
         (xref--show-xref-buffer
          (lambda () nil)
          `((fetched-xrefs . ,xrefs)
            (window . ,(embark--target-window))
            (auto-jump . ,xref-auto-jump-to-first-xref)
            (display-action))))))

(setf (alist-get 'consult-citre embark-exporters-alist)
      #'consult-citre--embark-export-xref)

(provide 'consult-citre)
