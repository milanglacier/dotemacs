;;; my-basics.el -*- lexical-binding: t; -*-

(setq user-full-name "Milan Glacier")
(setq user-mail-address "me@milanglacier.com")

(setq mac-right-option-modifier 'meta)
(setq mac-option-modifier 'meta)

(setq warning-minimum-level :error)

(setq initial-major-mode 'fundamental-mode)

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; try to disable the auto backup behavior
;; as much as possible
(setq make-backup-files nil
      create-lockfiles nil
      auto-save-default nil)

(set-display-table-slot standard-display-table 'truncation 32)
(set-display-table-slot standard-display-table 'wrap 32)

(add-hook 'prog-mode-hook #'my/display-truncation-and-wrap-indicator-as-whitespace)
(add-hook 'text-mode-hook #'my/display-truncation-and-wrap-indicator-as-whitespace)
;; by default when a long line is truncated, emacs displays
;; a "$" sign at the border of window, which is ugly,
;; replace "$" with " "
;; see discussion here: URL `https://emacs.stackexchange.com/questions/54817/remove-dollar-sign-at-beginning-of-line'

;; set default font
(add-to-list 'default-frame-alist
             '(font . "Monego Nerd Font Fix-15"))

(set-face-attribute 'variable-pitch nil :family "Bookerly" :height 160)

;; unless you have a really wide screen, always prefer
;; horizontal split (ale `evil-window-split')
(setq split-width-threshold 300)

;; When some commands ask you to input yes or no,
;; instead, y or n should be sufficient.
(setq use-short-answers t)

;; display line numbers in the left margin of the window.
(use-package display-line-numbers
    :init
    (setq display-line-numbers-type t)
    (global-display-line-numbers-mode)
    )

(use-package whitespace
    :init
    (setq whitespace-style '(face tabs tab-mark trailing))
    (global-whitespace-mode)
    )

(setq-default indent-tabs-mode nil)

;; smooth scroll
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(provide 'my-basics)
;;; my-basics ends here
