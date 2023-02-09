;;; my-init-tty.el -*- lexical-binding: t; -*-

(straight-use-package 'xclip)
(straight-use-package 'evil-terminal-cursor-changer)

(add-hook 'tty-setup-hook #'my/tty-setup)

(provide 'my-init-tty)
;;; my-tty.el ends here
