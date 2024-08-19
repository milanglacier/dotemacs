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


;; HACK: See doomemacs:
;; https://github.com/doomemacs/doomemacs/blob/d4ad14b75d090649c0783c41082ebc4d745b76a8/lisp/doom-start.el#L109

;; > I intentionally avoid calling `menu-bar-mode', `tool-bar-mode',
;; > and `scroll-bar-mode' because their manipulation of frame
;; > parameters ;; can trigger/queue a superfluous (and expensive,
;; > depending on the window system) frame redraw at startup. The
;; > variables must be set to `nil' as well so users don't have to
;; > call the functions twice to re-enable them.


;; My personal comment:
;; Setting `menu-bar-mode' to nil is required, as merely adjusting the
;; frame parameter is insufficient. While the menu bar may not be
;; visually displayed, it remains functionally present. This becomes
;; evident when attempting to click the top line using the mouse, as
;; unexpected pop-ups may occur, indicating the menu bar's invisible
;; existence. To completely disable the menu bar and prevent it from
;; being inadvertently activated, it is necessary to set
;; `menu-bar-mode' to nil.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(setq native-comp-async-report-warnings-errors 'silent)

(provide 'early-init)
;;; early-init.el ends here
