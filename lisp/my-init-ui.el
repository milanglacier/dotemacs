;;; my-init-ui.el -*- lexical-binding: t; -*-

(straight-use-package 'all-the-icons)
(straight-use-package 'doom-modeline)
(straight-use-package 'which-key)

(set-display-table-slot standard-display-table 'truncation 32)
(set-display-table-slot standard-display-table 'wrap 32)

(add-hook 'prog-mode-hook #'my/display-truncation-and-wrap-indicator-as-whitespace)
(add-hook 'text-mode-hook #'my/display-truncation-and-wrap-indicator-as-whitespace)
;; by default when a long line is truncated, emacs displays
;; a "$" sign at the border of window, which is ugly,
;; replace "$" with " "
;; see discussion here: URL `https://emacs.stackexchange.com/questions/54817/remove-dollar-sign-at-beginning-of-line'

;; set default font
(add-to-list 'default-frame-alist
             '(font . "Monego Nerd Font Fix-15"))

;; Set font for text that should be displayed more like prose.
(set-face-attribute 'variable-pitch nil :family "Bookerly" :height 160)

;; display line numbers in the left margin of the window.
(use-package display-line-numbers
    :init
    (setq display-line-numbers-type t)
    (global-display-line-numbers-mode)
    )

(use-package whitespace
    :init
    (setq whitespace-style '(face tabs tab-mark trailing))
    (global-whitespace-mode)
    )

(defvar my/side-window-slots
    '((helpful . 1) ;; 0 is the default
      (vterm . -1)
      (eldoc . 1)
      (aichat . 1)
      (python . -1)
      (R . -1)
      (Rhelp . 1)
      (Rdired . -1)
      (RWatch . -2)
      (xwidget-plot . -1)
      (dired-sidebar . -1))
    "The slot for different mode if used as side window,
this is for configuring `display-buffer-in-side-window',
configuring this would avoid buffer swallows other buffer's window
if they are side window.")

(defvar my/side-window-sides
    '((helpful . bottom) ;;bottom is the default
      (vterm . bottom)
      (eldoc . bottom)
      (aichat . bottom)
      (python . bottom)
      (R . bottom)
      (Rhelp . bottom)
      (Rdired . right)
      (RWatch . right)                  ;
      (xwidget-plot . right)
      (dired-sidebar . left)
      (pdf-outline . left))
    "The side different mode if used as side window,
this is for configuring `display-buffer-in-side-window',
configuring this would avoid buffer swallows other buffer's window
if they are side window.")

(setq window-combination-resize t
      ;; unless you have a really wide screen, always prefer
      ;; horizontal split (ale `split-window-below')
      split-width-threshold 300)

(blink-cursor-mode -1)

(use-package all-the-icons
    :if (display-graphic-p)
    :commands (all-the-icons-octicon
               all-the-icons-faicon
               all-the-icons-fileicon
               all-the-icons-wicon
               all-the-icons-material
               all-the-icons-alltheicon))

(use-package tab-bar
    :init
    (setq tab-bar-show 1
          tab-bar-close-button-show nil
          tab-bar-new-tab-choice "*scratch*"
          tab-bar-tab-hints t
          tab-bar-new-button-show nil
          tab-bar-format '(tab-bar-format-tabs-groups
                           tab-bar-separator))

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
        "C" #'tab-bar-close-group-tabs
        "o" #'tab-bar-close-other-tabs
        "]" #'tab-bar-switch-to-next-tab
        "[" #'tab-bar-switch-to-prev-tab
        "{" #'tab-bar-history-back
        "}" #'tab-bar-history-forward
        "b" #'tab-bar-move-window-to-tab
        ;; move current window to a new tab (break current tab)
        "l" #'tab-bar-move-tab ;; move tab to the right
        "h" #'tab-bar-move-tab-backward ;; move tab to the left
        "g" #'tab-bar-change-tab-group ;; make group
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

    (my/run-hook-once pre-command-hook tab-bar-history-mode)

    (advice-add #'tab-bar-new-tab :around #'my/set-scratch-directory)

    )

(use-package doom-modeline
    :init
    (add-hook 'doom-modeline-mode-hook #'size-indication-mode)
    (add-hook 'doom-modeline-mode-hook #'column-number-mode)
    (setq doom-modeline-bar-width 1
          doom-modeline-modal-icon nil
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

    (doom-modeline-mode 1))

(use-package which-key
    :init
    (my/run-hook-once pre-command-hook which-key-mode)

    :config
    (setq which-key-idle-delay 1
          which-key-popup-type 'minibuffer)
    )

(my~show-verses-at-startup-mode)

(provide 'my-init-ui)
;;; my-init-ui.el ends here
