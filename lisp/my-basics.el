;;; my-basics.el -*- lexical-binding: t; -*-

(setq user-full-name "Milan Glacier")
(setq user-mail-address "me@milanglacier.com")

(setq mac-right-option-modifier 'meta)
(setq mac-option-modifier 'meta)

(setq warning-minimum-level :error)

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(setq make-backup-files nil)

(set-display-table-slot standard-display-table 'truncation 32)

(defun my/display-truncation-indicator-as-whitespace ()
  (when (not (char-table-p buffer-display-table))
    (setq buffer-display-table (make-display-table)))
  (set-display-table-slot buffer-display-table 'truncation 32))

(add-hook 'prog-mode-hook #'my/display-truncation-indicator-as-whitespace)
(add-hook 'text-mode-hook #'my/display-truncation-indicator-as-whitespace)
;; by default when a long line is truncated, emacs displays
;; a "$" sign at the border of window, which is ugly,
;; replace "$" with " "
;; see discussion here: URL `https://emacs.stackexchange.com/questions/54817/remove-dollar-sign-at-beginning-of-line'

;; set default font
(add-to-list 'default-frame-alist
             '(font . "SpaceMono Nerd Font-15"))

;; When some commands ask you to input yes or no,
;; instead, y or n should be sufficient.
(setq use-short-answers t)

;; display line numbers in the left margin of the window.
(setq display-line-numbers-type t)
(global-display-line-numbers-mode)

(setq whitespace-style '(face tabs tab-mark trailing))
(global-whitespace-mode)
(setq-default indent-tabs-mode nil)

;; smooth scroll
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(provide 'my-basics)
;;; my-builtins ends here
