;;; init.el -*- lexical-binding: t; -*-

;; increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;; (setq user-init-file (or load-file-name (buffer-file-name)))
;; (setq user-emacs-directory (file-name-directory user-init-file))

(defvar my/config-dir (file-name-concat user-emacs-directory "lisp")
    "the directory of my configuration.")
(defvar my/autoloads-dir (file-name-concat my/config-dir "autoloads")
    "the directory of my autoloded functions.")
(defvar my/autoloads-file (file-name-concat my/autoloads-dir "loaddefs.el")
    "the directory of my autoloded functions.")

(push my/config-dir load-path)
(push my/autoloads-dir load-path)
(setq custom-file (file-name-concat user-emacs-directory "custom.el"))

;; bootstrap straight.el, copied from
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

(setq use-package-expand-minimally t
      use-package-always-ensure nil
      use-package-always-defer t
      debug-on-error t)

(defun my/update-all-autoloads ()
    (interactive)
    (when (not (file-exists-p my/autoloads-file))
        (with-current-buffer (find-file-noselect
                              my/autoloads-file)
            (insert ";;")
            (save-buffer)))
    (make-directory-autoloads my/autoloads-dir my/autoloads-file))

(load my/autoloads-file nil t)

(require 'my-init-utils)
(require 'my-basics)
(require 'my-init-ui)
(require 'my-init-colorscheme)
(require 'my-init-evil)
(require 'my-init-completion)
(require 'my-init-minibuffer)
(require 'my-init-vcs)
(require 'my-init-elisp)
(require 'my-init-org)
(require 'my-init-langs)
(require 'my-init-langtools)
(require 'my-init-os)
(require 'my-init-apps)
(require 'my-misc)

(setq debug-on-error nil)

;; after started up, reset GC threshold to normal.
(run-with-idle-timer 4 nil
                     (lambda ()
                         "Clean up gc."
                         (setq gc-cons-threshold  67108864) ; 64M
                         (setq gc-cons-percentage 0.1) ; original value
                         (garbage-collect)))

(provide 'init)
;;; init.el ends here
