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

;; don't need to input yes or no
;; y or n is sufficient.
(setq use-short-answers t)

;; Dont' make noises
(setq ring-bell-function #'ignore)

;; Press "TAB" key should not insert \t.
(setq-default indent-tabs-mode nil)

;; smooth scroll
(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil)

(provide 'my-basics)
;;; my-basics ends here
