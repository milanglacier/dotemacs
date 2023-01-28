;;; init.el --- Description -*- lexical-binding: t; -*-

;; increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;; (setq user-init-file (or load-file-name (buffer-file-name)))
;; (setq user-emacs-directory (file-name-directory user-init-file))

(add-to-list 'load-path (file-name-concat user-emacs-directory "lisp"))

(require 'my-init-utils)
(require 'my-basic-settings)
(require 'my-init-colorscheme)
(require 'my-init-tty)

(defun my/cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864) ; 64M
  (setq gc-cons-percentage 0.1) ; original value
  (garbage-collect))

;; after started up, reset GC threshold to normal.
(run-with-idle-timer 4 nil #'my/cleanup-gc)

(provide 'init)
;;; init.el ends here
