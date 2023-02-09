;;; my-langs-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/ess-set-company-backend ()
    (setq-local company-backends
                '((company-capf company-files
                                company-R-library company-R-args company-R-objects
                                :separate company-dabbrev
                                :with company-yasnippet))))
;;;###autoload
(defun my/ess-set-tab-width-4 ()
    (setq-local tab-width 4))

;;;###autoload (autoload #'my/send-region-to-ess "my-langs-autoloads" nil t)
(evil-define-operator my/send-region-to-ess (beg end)
    "This operator sends the region (either motion or text objects) to ess REPL"
    ;; t means don't echo the region in the ess REPL buffer
    (ess-eval-region beg end t))

;;;###autoload (autoload #'my/send-region-to-python "my-langs-autoloads" nil t)
(evil-define-operator my/send-region-to-python (beg end)
    (python-shell-send-region beg end t))

;;;###autoload
(defun my/poly-mode-disable-flymake (old-buf new-buf)
    "poly-mode are duplicated buffers with exactly the
same buffer content, when you are on `prog-mode' then your code linter
will be perplexed by those non-code content. So disable flymake in
poly-mode."
    (with-current-buffer new-buf
        (when flymake-mode
            (flymake-mode -1))))

(provide 'my-langs-autoloads)
;;; my-init-langs.el ends here
