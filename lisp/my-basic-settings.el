;;; my-basic-settings.el -*- lexical-binding: t; -*-

(setq user-full-name "Milan Glacier")
(setq user-mail-address "me@milanglacier.com")

(setq mac-right-option-modifier 'meta)
(setq mac-option-modifier 'meta)

(setq warning-minimum-level :error)

(set-display-table-slot standard-display-table 'truncation 32)

(defun my-display-truncation-indicator-as-whitespace ()
  (when (not (char-table-p buffer-display-table))
    (setq buffer-display-table (make-display-table)))
  (set-display-table-slot buffer-display-table 'truncation 32))

(add-hook 'prog-mode-hook #'my-display-truncation-indicator-as-whitespace)
(add-hook 'text-mode-hook #'my-display-truncation-indicator-as-whitespace)
;; by default when a long line is truncated, emacs displays
;; a "$" sign at the border of window, which is ugly,
;; replace "$" with " "
;; see discussion here: URL `https://emacs.stackexchange.com/questions/54817/remove-dollar-sign-at-beginning-of-line'

;; set default font
(add-to-list 'default-frame-alist
             '(font . "SpaceMono Nerd Font-15"))

;; display line numbers in the left margin of the window.
(setq display-line-numbers-type t)
(global-display-line-numbers-mode)

;; smooth scroll
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(provide 'my-basic-settings)
;;; my-init-basic-settings.el ends here
