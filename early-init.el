;;; early-init.el --- -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)

;; increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-startup-message t)

;; no menu bar, toolbar, scroll bar
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)))

(setq native-comp-async-report-warnings-errors 'silent)

(provide 'early-init)
;;; early-init.el ends here
