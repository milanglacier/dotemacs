;;; my-basic-settings.el -*- lexical-binding: t; -*-

(setq user-full-name "Milan Glacier")
(setq user-mail-address "me@milanglacier.com")

(setq mac-right-option-modifier 'meta)
(setq mac-option-modifier 'meta)

(setq warning-minimum-level :error)

(set-display-table-slot standard-display-table 'truncation 32)
;; by default when a long line is truncated, emacs displays
;; a "$" sign at the border of window, which is ugly,
;; replace "$" with " "
;; see discussion here: URL `https://emacs.stackexchange.com/questions/54817/remove-dollar-sign-at-beginning-of-line'

(add-to-list 'default-frame-alist
             '(font . "SpaceMono Nerd Font-15"))

(setq display-line-numbers-type t)
(global-display-line-numbers-mode)

(provide 'my-basic-settings)
;;; my-init-basic-settings.el ends here
