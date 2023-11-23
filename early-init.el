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

(setenv "PATH" (format "%s:%s:%s"
                       "/data/data/com.termux/files/usr/bin"
                       (getenv "PATH")
                       "/data/data/com.termux/files/home/.local/bin"))

(setenv "LD_LIBRARY_PATH" (format "%s%s"
                                  "/data/data/com.termux/files/usr/lib"
                                  (if-let ((ld-path (getenv "LD_LIBRARY_PATH")))
                                          (concat ":" ld-path)
                                      "")))

(setq exec-path `("/data/data/com.termux/files/usr/bin"
                  "/data/data/com.termux/files/home/.local/bin"
                  ,@exec-path))

(provide 'early-init)
;;; early-init.el ends here
