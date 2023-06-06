;;; my-evil-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/save-excursion-before-indenting (origin-fn &rest args)
    (save-excursion (apply origin-fn args)))

;; copied from doomemacs
;;;###autoload (autoload #'my/evil-apply-macro-line-by-line "my-evil-autoloads" nil t)
(evil-define-operator my/evil-apply-macro-line-by-line (beg end)
    "Apply macro to each line."
    :move-point nil
    (interactive "<r>")
    (let ((register (or evil-this-register (read-char)))
          macro)
        (cond ((or (and (eq register ?@) (eq evil-last-register ?:))
                   (eq register ?:))
               (setq macro (lambda () (evil-ex-repeat nil))
                     evil-last-register ?:))
              ((eq register ?@)
               (unless evil-last-register
                   (user-error "No previously executed keyboard macro."))
               (setq macro (evil-get-register evil-last-register t)))
              ((setq macro (evil-get-register register t)
                     evil-last-register register)))
        (unless macro
            (user-error "No macro recorded in %c register" register))
        (evil-change-state 'normal)
        (evil-with-single-undo
            (let ((lines (count-lines beg end)))
                (message "Applied macro in %c register %d times" register lines)
                (apply-macro-to-region-lines beg end macro)
                (message "Applied macro in %c register %d times...DONE" register lines)))))

;; adopted from
;; URL `https://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp'
;; NOTE: `make-symbol' will create new symbols even with the same name (they are different symbols)
;;;###autoload
(defmacro my/define-and-bind-paren-text-object (key start-regex end-regex)
    (let ((inner-name (gensym (concat "my/inner-" start-regex "-" end-regex "-text-obj")))
          (outer-name (gensym (concat "my/outer-" start-regex "-" end-regex "-text-obj"))))
        `(progn
             (evil-define-text-object ,inner-name (count &optional beg end type)
                 (evil-select-paren ,start-regex ,end-regex beg end type count nil))
             (evil-define-text-object ,outer-name (count &optional beg end type)
                 (evil-select-paren ,start-regex ,end-regex beg end type count t))
             (define-key evil-inner-text-objects-map ,key ',inner-name)
             (define-key evil-outer-text-objects-map ,key ',outer-name))))

;;;###autoload
(defmacro my/define-and-bind-local-paren-text-object (key start-regex end-regex hook)
    (let ((inner-name (gensym (concat "my/inner-" start-regex "-" end-regex "-text-obj")))
          (outer-name (gensym (concat "my/outer-" start-regex "-" end-regex "-text-obj")))
          (lambda-name (gensym (concat "my/lambda-" start-regex "-" end-regex "-text-obj"))))
        `(add-hook ',hook
                   (defun ,lambda-name ()
                       (evil-define-text-object ,inner-name (count &optional beg end type)
                           (evil-select-paren ,start-regex ,end-regex beg end type count nil))
                       (evil-define-text-object ,outer-name (count &optional beg end type)
                           (evil-select-paren ,start-regex ,end-regex beg end type count t))
                       (define-key evil-operator-state-local-map ,(concat "i" key) ',inner-name)
                       (define-key evil-operator-state-local-map ,(concat "a" key) ',outer-name)
                       (define-key evil-visual-state-local-map ,(concat "i" key) ',inner-name)
                       (define-key evil-visual-state-local-map ,(concat "a" key) ',outer-name)))))

;;;referenced from evil-collection-unimpaired.
(defconst my/SCM-conflict-marker "^\\(@@@ .* @@@\\|[<=>]\\{7\\}\\)"
    "A regexp to match SCM conflict marker.")

;;;referenced from evil-collection-unimpaired.
;;;###autoload (autoload #'my/previous-SCM-conflict-marker "my-evil-autoloads" nil t)
(evil-define-motion my/previous-SCM-conflict-marker (count)
    "Go to the previous SCM conflict marker or diff/patch hunk."
    :jump t
    (my/next-SCM-conflict-marker (- (or count 1))))

;;;referenced from evil-collection-unimpaired.
;;;###autoload (autoload #'my/next-SCM-conflict-marker "my-evil-autoloads" nil t)
(evil-define-motion my/next-SCM-conflict-marker (count)
    "Go to the next SCM conflict marker or diff/patch hunk."
    :jump t
    (evil-motion-loop (dir (or count 1))
        (cond
         ((> dir 0)
          (forward-line 1)
          (when (not (search-forward-regexp my/SCM-conflict-marker nil t))
              (forward-line -1))
          (move-beginning-of-line nil))
         (t
          (search-backward-regexp my/SCM-conflict-marker nil t)
          (move-beginning-of-line nil)))))

;;; shout out to `URL' https://github.com/noctuid/evil-textobj-anyblock#creating-more-specific-text-objects
;;;###autoload (autoload #'my:evil-textobj-anyblock-inner-quote "my-evil-autoloads" nil t)
(evil-define-text-object my:evil-textobj-anyblock-inner-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (require 'evil-textobj-anyblock)
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "'")
             ("“" . "”"))))
        (evil-textobj-anyblock--make-textobj beg end type count nil)))

;;;###autoload (autoload #'my:evil-apply-macro-line-by-line "my-evil-autoloads" nil t)
(evil-define-text-object my:evil-textobj-anyblock-outer-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (require 'evil-textobj-anyblock)
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "'")
             ("“" . "”"))))
        (evil-textobj-anyblock--make-textobj beg end type count t)))

(provide 'my-evil-autoloads)
;;; my-evil-autoloads ends here
