;;; my-init-org.el -*- lexical-binding: t; -*-

(straight-use-package 'evil-org)
;; (straight-use-package 'org-contrib)
;; (straight-use-package 'htmlize)
;; (straight-use-package 'jupyter)
;; (straight-use-package 'org-cliplink)
;; (straight-use-package 'orgit)
;; (straight-use-package 'ob-async)
;; (straight-use-package 'org-re-reveal)
;; (straight-use-package 'org-pdftools)
;; (straight-use-package 'org-cliplink)
;; (straight-use-package 'toc-org)
;; (straight-use-package 'ox-clip)
;; (straight-use-package 'avy)

;; (when IS-MAC
;;   (straight-use-package 'org-mac-link))

(use-package org
    :ensure nil
    :defer t
    :init
    (setq org-directory "~/Desktop/orgmode"
          org-agenda-files `(,org-directory)
          org-archive-location (expand-file-name "%s_archive::" (concat org-directory "/archive"))
          org-id-locations-file (file-name-concat org-directory ".orgids")
          org-highlight-latex-and-related '(native latex script entities)
          org-special-ctrl-a/e t
          org-M-RET-may-split-line nil
          org-insert-heading-respect-content t
          org-preview-latex-image-directory (file-name-concat "~/.cache" "ltximg/")
          org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "WAIT(w)" "HOLD(h)"
                                        "|" "DONE(d)" "KILL(k)")))

    ;; turn off evil-vimish, use org builtin fold
    (add-hook 'org-mode-hook (defun my/turn-off-evil-vimish () (evil-vimish-fold-mode -1)))

    :config
    (evil-set-initial-state 'org-agenda-mode 'motion)
    (general-define-key
     :states 'normal
     :keymaps 'org-mode-map
     "TAB" #'org-cycle)

    )

(use-package evil-org
    :hook (org-mode . evil-org-mode)

    :init
    (add-hook 'org-capture-mode-hook #'evil-insert-state)

    :config
    (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
    (evil-org-set-key-theme))

(use-package evil-org-agenda
    :after org-agenda
    :config
    (evil-org-agenda-set-keys))


(provide 'my-init-org)
;;; my-init-ui.el ends here
