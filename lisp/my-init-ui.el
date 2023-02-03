;;; my-init-ui.el -*- lexical-binding: t; -*-

(straight-use-package 'all-the-icons)
(straight-use-package 'doom-modeline)
(straight-use-package 'which-key)

(use-package all-the-icons
    :if (display-graphic-p)
    :commands (all-the-icons-octicon
               all-the-icons-faicon
               all-the-icons-fileicon
               all-the-icons-wicon
               all-the-icons-material
               all-the-icons-alltheicon))

(use-package tab-bar
    :defer t
    :ensure nil
    :init
    (setq tab-bar-show 1)
    (setq tab-bar-close-button-show nil)
    (setq tab-bar-new-tab-choice "*scratch*")
    (setq tab-bar-tab-hints t)
    (setq tab-bar-new-button-show nil)
    (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

    :config
    (defmacro my/tab-bar-go-to-tab-macro (number)
        (let ((fun (intern (format "my/tab-bar-go-to-tab-%d" number))))
            `(defun ,fun ()
                 ,(format "go to tab %d" number)
                 (interactive)
                 (tab-bar-select-tab ,number))))

    (general-create-definer my/tab-map
        :prefix "SPC TAB"
        :non-normal-prefix "M-SPC TAB"
        :prefix-map 'my/tab-map)

    (my/tab-map
        :states '(motion insert normal)
        :keymaps 'override
        "" '(:ignore t :which-key "Tab")
        "n" #'tab-bar-new-tab
        "c" #'tab-bar-close-tab
        "o" #'tab-bar-close-other-tabs
        "]" #'tab-bar-switch-to-next-tab
        "[" #'tab-bar-switch-to-prev-tab
        "TAB" #'tab-bar-switch-to-tab
        "1" (my/tab-bar-go-to-tab-macro 1)
        "2" (my/tab-bar-go-to-tab-macro 2)
        "3" (my/tab-bar-go-to-tab-macro 3)
        "4" (my/tab-bar-go-to-tab-macro 4)
        "5" (my/tab-bar-go-to-tab-macro 5)
        "6" (my/tab-bar-go-to-tab-macro 6)
        "7" (my/tab-bar-go-to-tab-macro 7)
        "8" (my/tab-bar-go-to-tab-macro 8)
        "9" (my/tab-bar-go-to-tab-macro 9))

    )

(use-package doom-modeline
    :hook (after-init . doom-modeline-mode)

    :init
    (add-hook 'doom-modeline-mode-hook #'size-indication-mode)
    (add-hook 'doom-modeline-mode-hook #'column-number-mode)
    (setq doom-modeline-bar-width 3
          doom-modeline-github nil
          doom-modeline-mu4e nil
          doom-modeline-persp-name nil
          doom-modeline-minor-modes nil
          ;; doom-modeline-major-mode-icon nil
          doom-modeline-buffer-file-name-style 'relative-from-project
          ;; Only show file encoding if it's non-UTF-8 and different line endings
          ;; than the current OSes preference
          doom-modeline-buffer-encoding 'nondefault
          doom-modeline-default-eol-type (cond (IS-MAC 2)
                                               (IS-WINDOWS 1)
                                               (0)))
    )

(use-package which-key
    :hook (after-init . which-key-mode)

    :config
    (setq which-key-idle-delay 0.4))

(provide 'my-init-ui)
;;; my-init-ui.el ends here
