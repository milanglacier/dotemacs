;;; init.el -*- lexical-binding: t; -*-

;; increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

(defvar mg-config-dir (file-name-concat user-emacs-directory "lisp" "config")
    "the directory of my configuration.")
(defvar mg-lib-dir (file-name-concat user-emacs-directory "lisp" "lib")
    "the directory of my library functions")
(defvar mg-site-lisp-dir (file-name-concat user-emacs-directory "site-lisp")
    "the root directory of third-party lisp files.")
(defvar mg-site-lisp-dirs `(,mg-site-lisp-dir)
    "the directories of third-party lisp files.")

(defvar mg-autoloads-file (file-name-concat user-emacs-directory "generated-loaddefs" "lib-loaddefs.el")
    "the file of my generated autoload definitions")

(defvar mg-site-lisp-autoloads-file
    (file-name-concat user-emacs-directory "generated-loaddefs" "site-lisp-loaddefs.el")
    "the file of third-party autoloaded functions.")

(push mg-config-dir load-path)
(push mg-lib-dir load-path)
(dolist (dir mg-site-lisp-dirs) (push dir load-path))
(setq custom-file (file-name-concat user-emacs-directory "custom.el"))

(setq use-package-expand-minimally t
      ;; use-package is a macro. Don't let the macro expands into
      ;; codes with too much of irrelevant checks.
      ;; Straight is my package manager, don't let package.el get
      ;; involved.
      use-package-always-defer t
      ;; This is a useful trick to speed up your startup time. Only
      ;; use `require' when it's necessary. By setting the
      ;; `use-package-always-defer' option to t, use-package won't
      ;; call `require' in all cases unless you explicitly include
      ;; :demand t'. This will prevent unnecessary package loading and
      ;; speed up your Emacs startup time.
      straight-check-for-modifications nil ;;'(find-at-startup)
      ;; This is a useful trick to further optimize your startup
      ;; time. Instead of using `straight-check-for-modifications' to
      ;; check if a package has been modified, you can manually
      ;; rebuild the package by `straight-rebuild-package' when you
      ;; know its source code has changed. This avoids the overhead of
      ;; the check. Make sure you know what you are doing here when
      ;; setting this option.
      debug-on-error t)

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

(load mg-autoloads-file nil t) ;; don't show message
(load mg-site-lisp-autoloads-file nil t)

(require 'config-utils)
(require 'config-basics)
(require 'config-ui)
(require 'config-colorscheme)
(require 'config-evil)
(require 'config-completion)
(require 'config-minibuffer)
(require 'config-vcs)
(require 'config-elisp)
(require 'config-org)
(require 'config-langs)
(require 'config-langtools)
(require 'config-os)
(require 'config-apps)
(require 'config-email)
(require 'config-misc)

;; I personally HATE custom.el. But I don't think I have a better
;; place to store some temp file.
(when (file-exists-p custom-file)
    (load custom-file nil t))

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
