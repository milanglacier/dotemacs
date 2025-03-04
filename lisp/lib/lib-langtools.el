;;; lib-langtools.el -*- lexical-binding: t; -*-

(defalias #'mg-eglot-citre-capf
    (cape-capf-super #'eglot-completion-at-point #'citre-completion-at-point))

;;;###autoload
(defun mg-toggle-citre-eglot-capf ()
    (if (eglot-managed-p)
            (add-to-list 'completion-at-point-functions #'mg-eglot-citre-capf)
        (setq-local completion-at-point-functions
                    (delq #'mg-eglot-citre-capf completion-at-point-functions))))

(defun mg-eldoc-buffer-dwim-fallback ()
    "When eldoc buffer window is not opened, display the eldoc
window. Pressing \\[mg-eldoc-buffer-dwim] again within a short
period (1s currently as hard coded) will move your focus on the eldoc
window. If the shorter period has gone, calling this command will
close the eldoc window."
    (interactive)
    (if-let* ((eldoc-win (get-buffer-window "*eldoc*")))
            (delete-window eldoc-win)
        (progn
            (eldoc-doc-buffer t)
            (mg-eldoc-dwim-hack)))
    )

;;;###autoload (autoload #'mg-eldoc-buffer-dwim "lib-langtools" nil t)
(defalias #'mg-eldoc-buffer-dwim #'mg-eldoc-buffer-dwim-fallback)

(defun mg-eldoc-dwim-hack ()
    "Alias `mg-eldoc-buffer-dwim' to `mg-eldoc-focus'. And after the
period, revert the action."
    (defalias #'mg-eldoc-buffer-dwim #'mg-eldoc-focus)
    (run-with-idle-timer 1 nil #'mg-eldoc-dwim-revert))

(defun mg-eldoc-dwim-revert ()
    (defalias #'mg-eldoc-buffer-dwim #'mg-eldoc-buffer-dwim-fallback))

(defun mg-eldoc-focus ()
    "focus on the eldoc window"
    (interactive)
    (when (get-buffer-window "*eldoc*")
        (select-window (get-buffer-window "*eldoc*"))))

;;;###autoload
(defmacro mg-xref-move-in-original-src-macro (func)
    "There can only be one xref buffer. That is, if you find
references of other symbol the previous one will be overwritten. The
official `xref-next-line' `xref-next-group' only allows you to move
the location in the src buffer when your point is in the xref buffer
window. This macro creates funcs that allow you to move current window
to next xref location."
    (let ((xref-move-func (intern (format "mg-%s" func)))
          (xref-move-func-desc (format "Effectively calling %s in the src window." func)))
        `(defun ,xref-move-func ()
             ,xref-move-func-desc
             (interactive)
             (with-current-buffer "*xref*"
                 (funcall #',func)))))

;; TODO: Utilize the contextual information from previous code blocks
;; PLAN:
;; 1. Get the contents of previous code blocks
;; 2. Paste the code blocks into the temp buffer
;; 3. When finish editing, delete those inserted contextual code.

(defvar mg-code-block-lsp-major-mode
    '(ess-r-mode python-ts-mode)
    "the major mode that will activate lsp in code blocks (either in
markdown or in org)")

;;;###autoload
(defun mg-markdown-src-lsp-setup()
    "eglot requires the buffer to be a file to be able to attach to
the lsp. Thus the indirect buffer created by `edit-indirect' needs to
be associated with a real file."
    (setq-local buffer-file-name (file-name-concat default-directory "indirect-src.tmp"))
    (dolist (mode mg-code-block-lsp-major-mode)
        (when (derived-mode-p mode)
            (eglot-ensure))))

;;; copied from Centaur Emacs
;;;###autoload
(defmacro mg-org-babel-lsp-setup (lang)
    "Support LANG in org source code block."
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (mg-setup (intern (format "mg-lsp-setup-for--%s" (symbol-name edit-pre)))))
        `(progn
             (defun ,mg-setup (info)
                 (setq buffer-file-name
                       (or (->> info caddr (alist-get :file))
                           (file-name-concat default-directory "org-babel-src.tmp")))
                 (dolist (mode mg-code-block-lsp-major-mode)
                     (when (derived-mode-p mode)
                         (eglot-ensure))))

             (if (fboundp #',edit-pre)
                     (advice-add #',edit-pre :after #',mg-setup)
                 (progn
                     (defun ,edit-pre (info)
                         (,mg-setup info)))))))

;;;###autoload
(defun mg-treesit-install-all-language-grammar ()
    (interactive)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

(defvar major-mode-reformatter-plist
    '(python-ts-mode black-format-buffer
                     python-mode black-format-buffer
                     sql-mode sql-formatter-format-buffer)
    "A plist of major modes and their corresponding reformatters.")

;;;###autoload
(defun mg-formatter ()
    "If current LSP has a formatter, use it. Otherwise, use the
reformatter according to the `major-mode-reformatter-plist'"
    (interactive)
    (if (and (eglot-managed-p)
             (eglot--server-capable :documentFormattingProvider))
            (call-interactively #'eglot-format)
        (when-let* ((formatter (plist-get major-mode-reformatter-plist major-mode)))
            (call-interactively formatter))))

;;;###autoload
(defun mg-dape-start-or-continue ()
    "Try `dape-continue' and fall back to `dape'."
    (interactive)
    (require 'dape)
    (condition-case err
            (call-interactively #'dape-continue)
        (error (call-interactively #'dape))))

(defun mg-dape-info-goto-prev-tab  ()
    (interactive)
    (dape--info-buffer-tab t))

(defun mg-dape-info-goto-next-tab  ()
    (interactive)
    (dape--info-buffer-tab))

;;;###autoload
(defun mg--dape-keymap-setup ()
    (general-define-key
     :keymaps 'local
     "<f5>" #'mg-dape-start-or-continue
     "<S-f5>" #'dape-quit
     "<f6>" #'dape-pause
     "<f9>" #'dape-breakpoint-toggle
     "<S-f9>" #'dape-breakpoint-expression
     "<f10>" #'dape-next ;; step-over
     "<f11>" #'dape-step-in
     "<S-f11>" #'dape-step-out)
    )

(provide 'lib-langtools)
;;; lib-langtools.el ends here
