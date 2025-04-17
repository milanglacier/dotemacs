;;; config-ui.el -*- lexical-binding: t; -*-

(straight-use-package 'doom-modeline)
(straight-use-package 'which-key)
(straight-use-package 'nerd-icons)

(set-display-table-slot standard-display-table 'truncation 32)
(set-display-table-slot standard-display-table 'wrap 32)

(add-hook 'prog-mode-hook #'mg-display-truncation-and-wrap-indicator-as-whitespace)
(add-hook 'text-mode-hook #'mg-display-truncation-and-wrap-indicator-as-whitespace)
;; by default when a long line is truncated, emacs displays
;; a "$" sign at the border of window, which is ugly,
;; replace "$" with " "
;; see discussion here: URL `https://emacs.stackexchange.com/questions/54817/remove-dollar-sign-at-beginning-of-line'

;; set default font
(add-to-list 'default-frame-alist
             '(font . "Monofur Nerd Font Mono-20"))

;; Set font for text that should be displayed more like prose.
(set-face-attribute 'variable-pitch nil :family "Bookerly" :height 160)

;; optimize performance for long lines. Don't set these options when
;; you are using right-to-left languages like Arabic.
;; See URL `https://emacs-china.org/t/topic/25811/9'
(setq-default bidi-inhibit-bpa t
              bidi-display-reordering nil
              long-line-threshold 1000
              large-hscroll-threshold 1000
              syntax-wholeline-max 1000
              use-dialog-box nil)

;; display line numbers in the left margin of the window.
(use-package display-line-numbers
    :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
    :init
    (setq display-line-numbers-type t)
    (mg-toggle-map
        :keymaps 'override
        :states '(motion insert normal)
        "n" #'display-line-numbers-mode)
    )

(use-package whitespace
    :init
    (setq whitespace-style '(face tabs tab-mark trailing))
    :hook ((prog-mode text-mode conf-mode) . whitespace-mode))

(defvar mg-side-window-slots
    '((helpful . 1) ;; 0 is the default
      (vterm . -1)
      (eat . -1)
      (eldoc . 1)
      (aichat . 2)
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

(defvar mg-side-window-sides
    '((helpful . bottom) ;;bottom is the default
      (vterm . bottom)
      (eat . bottom)
      (eldoc . bottom)
      (aichat . bottom)
      (Rhelp . bottom)
      (Rdired . right)
      (RWatch . right)
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

(use-package tab-bar
    :init
    (setq tab-bar-show 1
          ;; hide the tab bar when it has only one tab, and show it
          ;; again once more tabs are created.
          tab-bar-close-button-show nil
          ;; Previously, this option was set to `"*scratch*"`, which
          ;; would create a buffer associated with a file (named
          ;; `*scratch*`) if that buffer did not exist. This behavior
          ;; was not ideal, as one might accidentally save that file,
          ;; resulting in directory pollution. Set this option to a
          ;; function that creates a buffer and avoid associating it
          ;; with a file.
          tab-bar-new-tab-choice #'mg--tab-bar-new-buffer
          tab-bar-tab-hints t
          tab-bar-new-button-show nil
          tab-bar-separator " "
          ;; We have our own tab-bar UI that includes a variable for
          ;; configuring the separator. Thus, we want to ensure the
          ;; tab bar does not automatically resize, to keep the
          ;; tab-bar ui consistent.
          tab-bar-auto-width nil
          tab-bar-format '(tab-bar-format-tabs-groups)
          ;; remove tab-bar-tab-name-format-close-button from this
          ;; hook as we don't need the close button
          tab-bar-tab-name-format-functions '(tab-bar-tab-name-format-hints
                                              mg--tab-bar-add-custom-boundaries
                                              tab-bar-tab-name-format-face)
          tab-bar-tab-group-format-function #'mg--tab-bar-tab-group-format)

    (general-create-definer mg-tab-map
        :prefix "SPC TAB"
        :non-normal-prefix "M-SPC TAB"
        :prefix-map 'mg-tab-map)

    (mg-tab-map
        :states '(motion insert normal emacs)
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
        "1" (mg-tab-bar-go-to-tab-macro 1)
        "2" (mg-tab-bar-go-to-tab-macro 2)
        "3" (mg-tab-bar-go-to-tab-macro 3)
        "4" (mg-tab-bar-go-to-tab-macro 4)
        "5" (mg-tab-bar-go-to-tab-macro 5)
        "6" (mg-tab-bar-go-to-tab-macro 6)
        "7" (mg-tab-bar-go-to-tab-macro 7)
        "8" (mg-tab-bar-go-to-tab-macro 8)
        "9" (mg-tab-bar-go-to-tab-macro 9))

    (mg-run-hook-once pre-command-hook tab-bar-history-mode)

    (advice-add #'tab-bar-new-tab :around #'mg-set-scratch-directory)

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
          doom-modeline-buffer-file-name-style 'relative-from-project
          ;; Only show file encoding if it's non-UTF-8 and different line endings
          ;; than the current OSes preference
          doom-modeline-buffer-encoding 'nondefault
          doom-modeline-default-eol-type (cond (IS-MAC 2)
                                               (IS-WINDOWS 1)
                                               (0)))
    (mg-run-hook-once (minibuffer-setup-hook find-file-hook special-mode-hook) doom-modeline-mode)
    )

(use-package which-key
    :init
    (mg-run-hook-once pre-command-hook which-key-mode)

    :config
    (setq which-key-idle-delay 1
          which-key-popup-type 'minibuffer)
    )

(mg--welcome-screen-mode)

(provide 'config-ui)
;;; config-ui.el ends here
