;;; my-iniy-elisp.el -*- lexical-binding: t; -*-

(straight-use-package 'helpful)

(use-package helpful
  :defer t
  :init
  (general-define-key
   [remap describe-function] #'helpful-callable
   [remap describe-command] #'helpful-command
   [remap describe-variable] #'helpful-variable
   [remap describe-key] #'helpful-key
   [remap describe-symbol] #'helpful-symbol))

(provide 'my-init-elisp)
;; my-init-elisp.el ends here
