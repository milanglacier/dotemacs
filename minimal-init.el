;;; minimal-init.el -*- lexical-binding: t; -*-

;; this is for debugging purpose

(require 'package)
(setq package-user-dir "~/Downloads/emacs-package-dir")

(setq package-list '())
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq warning-minimum-level :error)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(xterm-mouse-mode 1)

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
