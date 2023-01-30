;;; init.el -*- lexical-binding: t; -*-

;; increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;; (setq user-init-file (or load-file-name (buffer-file-name)))
;; (setq user-emacs-directory (file-name-directory user-init-file))

(add-to-list 'load-path (file-name-concat user-emacs-directory "lisp"))

;; bootsrap straight, copied from
;; URL: `https://github.com/radian-software/straight.el#getting-started'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(require 'my-init-utils)
(require 'my-basic-settings)
(require 'my-init-colorscheme)
(require 'my-init-tty)
(require 'my-init-evil)

(defun my/cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864) ; 64M
  (setq gc-cons-percentage 0.1) ; original value
  (garbage-collect))

;; after started up, reset GC threshold to normal.
(run-with-idle-timer 4 nil #'my/cleanup-gc)

(provide 'init)
;;; init.el ends here
