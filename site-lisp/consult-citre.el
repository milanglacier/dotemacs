;;; consult-citre.el -*- lexical-binding: t; -*-
;; author: Chingyat
;; URL `https://emacs-china.org/t/citre-ctags/17604/666'
;; URL `https://emacs-china.org/t/citre-ctags/17604/669'

(require 'citre)
(require 'consult)
(require 'consult-xref)

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
                  nil)
                 opts)
         hl)
        ))

(defun consult-citre-readtags--format (lines)
    (let ((root (citre-project-root))
          (info (citre-readtags-tags-file-info (citre-tags-file-path))))
        (mapcar (lambda (line)
                    (let* ((tag (citre-readtags--parse-line
                                 line
                                 info
                                 '(name input pattern line kind) '() '()
                                 '(ext-abspath ext-kind-full) '() '() t
                                 ))
                           (xref (citre-xref--make-object tag))
                           (loc (xref-item-location xref))
                           (group (if (fboundp 'xref--group-name-for-display)
                                          ;; This function is available in xref 1.3.2
                                          (xref--group-name-for-display
                                           (xref-location-group loc) root)
                                      (xref-location-group loc)))
                           (cand (consult--format-file-line-match
                                  group
                                  (or (xref-location-line loc) 0)
                                  (xref-item-summary xref))))
                        (add-text-properties 0 1 `(consult-xref ,xref consult--prefix-group ,group) cand)
                        cand))
                lines)))

;;;###autoload
(defun consult-citre (initial)
    "Read a tag from minibuffer and jump to the tag."
    (interactive "P")
    (let* ((candidates  (consult--async-command
                         #'consult-citre-readtags--builder
                         (consult--async-transform consult-citre-readtags--format)
                         (consult--async-highlight #'consult-citre-readtags--builder)))
           (consult-xref--fetcher (lambda ()
                                      (mapcar (apply-partially get-text-property 0 'consult-xref cand)
                                              (funcall candidates 'candidates)))))
        (xref-pop-to-location
         (consult--read
          candidates
          :prompt "Tag: "
          :keymap consult-async-map
          :require-match t
          :category 'consult-xref
          :initial (consult--async-split-initial initial)
          :group #'consult--prefix-group
          :state
          ;; do not preview other frame
          (consult-xref--preview #'switch-to-buffer)
          :lookup (apply-partially #'consult--lookup-prop 'consult-xref)))))

(provide 'consult-citre)
