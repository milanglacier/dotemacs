;;; dashverse.el --- Display elegant verses on the Emacs startup screen -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Milan Glacier <dev@milanglacier.com>
;; Keywords: startup, dashboard

;;; Commentary:

;; This package provides an elegant and minimalist startup screen for
;; Emacs, replacing the default `*scratch*` buffer message with a
;; centered display of poetry.
;;
;; The screen is divided into three sections:
;;
;; 1. A randomly selected verse at the top.
;; 2. A list of customizable, quick-access actions in the middle.
;; 3. Another randomly selected verse at the bottom.
;;
;; To use, simply activate `startverse--welcome-screen-mode' in your
;; init file.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'general)

(defvar startverse-header-verses
    '(("Bright star, would I were steadfast as thee art!"
       " John Keats")
      ("For clattering parrots to launch their fleet at sunrise"
       "For April to ignite the African violet"
       " Derek Walcott")
      ("In these poinsettia meadows of her tides,—"
       "Adagios of islands, O my Prodigal,"
       "Complete the dark confessions her veins spell."
       " Hart Crane")
      ("帝子降兮北渚，目眇眇兮愁予，"
       "袅袅兮秋风，洞庭波兮木叶下。"
       " 《湘夫人》")
      ("美人迈兮音尘阙，隔千里兮共明月。"
       "临风叹兮将焉歇，川路长兮不可越！"
       " 《月赋》")
      ("浴兰汤兮沐芳，华采衣兮若英。"
       "灵连蜷兮既留，烂昭昭兮未央。"
       "蹇将憺兮寿宫，与日月兮齐光。"
       " 《云中君》"))
    "The verses displayed on the top of `initial-scratch-message'.")

(defvar startverse-foot-verses
    '(("Whispers antiphonal in the azure swing..."
       " Hart Crane")
      ("In the drumming world that dampens your tired eyes"
       "Behind two clouding lenses, sunrise, sunset,"
       "The quiet ravage of diabetes."
       " Derek Walcott")
      ("What words"
       "Can strangle this deaf moonlight? For we"
       "Are overtaken."
       " Hart Crane")
      ("搴汀洲兮杜若，将以遗兮远者。"
       "时不可兮骤得，聊逍遥兮容与！"
       " 《湘夫人》")
      ("月既没兮露欲晞，岁方晏兮无与归。"
       "佳期可以还，微霜沾人衣。"
       " 《月赋》")
      ("雷填填兮雨冥冥，猨啾啾兮狖夜鸣。"
       "风飒飒兮木萧萧，思公子兮徒离忧。"
       " 《山鬼》"))
    "The verses displayed on the bottom of `initial-scratch-message'.")

(defvar startverse-actions
    '((" New [T]heme      " . load-theme)
      (" New [V]erse      " . startverse-refresh-verses)
      (" [S]tartup Time   " . startverse-emacs-startup-time)
      (" Org [A]genda     " . org-agenda-list)
      (" Recent [F]iles   " . consult-recent-file)
      (" Recent [P]rojects" . project-switch-project))
    "The actions to be displayed on the welcome screen.")

(defvar startverse--welcome-screen-setup-time nil
    "Timestamp recorded after the welcome buffer finishes rendering.")

(defun startverse--empty-lines-between-sections ()
    (let ((height (window-height)))
        (cond ((< height 40) 3)
              ((< height 50) 8)
              (t 10))))

(defun startverse--top-lines-padding (content-length)
    (let* ((height (window-height))
           (paddings (ceiling (* (- height content-length) 0.5)))
           (paddings (max 0 paddings)))
        (make-string paddings ?\n)))

(defun startverse--right-margin-when-centering-margin ()
    "The absolute symmetry is in fact less aesthetically pleasing than
a slight leftward skew."
    (let ((width (window-width)))
        (cond ((< width 100) width)
              ((< width 200) (- width 10))
              (t (- width 15)))))

;;;###autoload
(defun startverse-emacs-startup-time ()
    "Measure the startup time until the welcome screen is displayed.
More accurate than `emacs-init-time'."
    (interactive)
    (message (format "%f seconds"
                     (max (float-time (time-subtract startverse--welcome-screen-setup-time before-init-time))
                          (float-time (time-subtract after-init-time before-init-time))))))

(defun startverse--welcome-screen-set-keymap ()
    (let* ((action-strings (mapcar #'car startverse-actions))
           (actions (mapcar #'cdr startverse-actions))
           (keys (mapcar (lambda (x)
                             (when-let* ((pos (string-search "[" x)))
                                 (substring x (1+ pos) (+ 2 pos))))
                         action-strings))
           (keymaps
            (cl-loop for key in keys
                     for action in actions
                     if key
                     append `(,(downcase key) ,action))))
        (apply #'general-define-key :keymaps 'local :states '(normal emacs) keymaps)))

(defface startverse-verses
    '((((background light)) :foreground "#ed80b5" :slant italic)
      (((background dark)) :foreground "#a070b5" :slant italic))
    "Faces used for welcome screen verses.")

(defface startverse-verse-quotes
    '((((background light)) :foreground "#e8ae92")
      (((background dark)) :foreground "#ad7f2a"))
    "Faces used for the quotations on the welcome screen.")

(defface startverse-welcome-screen-action
    '((((background light)) :foreground "#0398fc")
      (((background dark)) :foreground "#73915e"))
    "Faces used for the welcome screen actions.")

(defface startverse-welcome-screen-action-key
    '((((background light)) :foreground "#6b82e2")
      (((background dark)) :foreground "#cfe8a3"))
    "Faces applied to the action key hints on the welcome screen.")

(defun startverse--generate-initial-messages ()
    (let* ((head-verse (nth (random (length startverse-header-verses))
                            startverse-header-verses))
           (foot-verse (nth (random (length startverse-foot-verses))
                            startverse-foot-verses))
           (action-strings (mapcar #'car startverse-actions))
           (n-lines-between-section (startverse--empty-lines-between-sections))
           ;; Require one additional \n inserted at the end of other sections.
           (lines-between-sections
            (make-string (1+ n-lines-between-section) ?\n))
           (content-length (+ (length head-verse)
                              (length foot-verse)
                              (length action-strings)
                              (* 2 n-lines-between-section)))
           (top-paddings (startverse--top-lines-padding content-length)))
        (setq head-verse (mapconcat #'startverse--center-a-line head-verse "\n"))
        (setq action-strings (mapconcat #'startverse--center-a-line action-strings "\n"))
        (setq foot-verse (mapconcat #'startverse--center-a-line foot-verse "\n"))
        (concat top-paddings
                head-verse
                lines-between-sections
                action-strings
                lines-between-sections
                foot-verse)))

(defun startverse--center-a-line (x)
    "Center one line of verse or action string."
    (let ((spaces-to-be-inserted
           (/ (- (startverse--right-margin-when-centering-margin) (string-width x))
              2)))
        (concat (make-string spaces-to-be-inserted ?\ )
                x)))

(defun startverse--verses-add-font-lock ()
    (font-lock-add-keywords
     nil
     '(("^ *\\([^\"]+\\)$" 1 'startverse-verses)
       ("^ *\\(.+\\)$" 1 'startverse-verse-quotes)
       ("^ *\\(.+\\)\\[" 1 'startverse-welcome-screen-action)
       ("\\(\\[.*\\]\\)" 1 'startverse-welcome-screen-action-key)
       ("\\]\\(.*\\)$" 1 'startverse-welcome-screen-action))))

;;;###autoload
(defun startverse--welcome-screen-mode ()
    (setq initial-scratch-message nil)
    (add-hook 'emacs-startup-hook #'startverse--set-welcome-screen-buffer)
    ;; When running emacs in server mode, cannot get the window
    ;; height/width at startup.
    (add-hook 'server-after-make-frame-hook #'startverse-refresh-verses))

(defun startverse--set-welcome-screen-buffer ()
    (with-current-buffer "*scratch*"
        (insert (startverse--generate-initial-messages))
        (startverse--verses-add-font-lock)
        (setq-local mode-line-format nil)
        (startverse--welcome-screen-set-keymap))
    (setq startverse--welcome-screen-setup-time (current-time)))

;;;###autoload
(defun startverse-refresh-verses ()
    "Refresh verses in the scratch buffer."
    (interactive)
    (with-current-buffer "*scratch*"
        (erase-buffer)
        (insert (startverse--generate-initial-messages))))

(provide 'dashverse)
;;; dashverse.el ends here
