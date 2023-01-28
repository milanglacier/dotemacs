;;; my-init-utils.el -*- lexical-binding: t; -*-

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

(straight-use-package '(use-package :host github :repo "jwiegley/use-package"))
(straight-use-package '(general.el :host github :repo "noctuid/general.el"))

(provide 'my-init-utils)
;;; my-init-utils.el ends here
