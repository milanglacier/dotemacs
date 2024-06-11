;;; my-ui-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/display-truncation-and-wrap-indicator-as-whitespace ()
    (when (not (char-table-p buffer-display-table))
        (setq buffer-display-table (make-display-table)))
    (set-display-table-slot buffer-display-table 'truncation 32)
    (set-display-table-slot buffer-display-table 'wrap 32))

;;;###autoload
(defmacro my/tab-bar-go-to-tab-macro (number)
    (let ((fun (intern (format "my/tab-bar-go-to-tab-%d" number))))
        `(defun ,fun ()
             ,(format "go to tab %d" number)
             (interactive)
             (tab-bar-select-tab ,number))))

;;;###autoload
(defun my/set-scratch-directory (old-fun &rest args)
    "After creating a new tab, the default buffer to be displayed is
scratch buffer whose directory is set to where emacs is initialized.
Change it to the directory of previous buffer where `tab-bar-new-tab'
is called."
    (let ((current-dir default-directory))
        (apply old-fun args)
        (setq-local default-directory current-dir)))

;;;###autoload
(defun my:font-set-small-mono-font ()
    "Set the default font to a smaller sized font for current buffer."
    (face-remap-add-relative 'default :height 140))

;;;###autoload
(defun my:font-set-small-variable-font ()
    "Set the default font to a smaller sized font for current buffer."
    (face-remap-add-relative 'default :height 140 :family "Bookerly"))

(defvar my$header-verses
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
    "the verses displayed on the top of `initial-scratch-message'")

(defvar my$foot-verses
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
    "the verses displayed on the bottom of `initial-scratch-message'")

(defvar my$actions
    '((" Emacs Init Time        " . emacs-init-time)
      (" Pick New Theme         " . my:theme-set-dynamically)
      (" Pick New Verse         " . my~refresh-verses)
      (" Org Agenda      SPC o a" . org-agenda-list)
      (" Recent Files    SPC f o" . consult-recent-file)
      (" Recent Projects SPC f p" . project-switch-project))
    "the actions to be displayed on the welcome screen")

(defvar my$empty-lines-between-sections 10)

(defvar my$right-margin-when-centering-margin 80
    "The assumed window width to calculate appropriate number of
whitespaces to be prepended when centering the verses.")

(defface my&verses
    '((((background light)) :foreground "#ed80b5" :slant italic)
      (((background dark)) :foreground "#a070b5" :slant italic))
    "the faces used for the verses")

(defface my&verse-quotes
    '((((background light)) :foreground "#e8ae92")
      (((background dark)) :foreground "#ad7f2a"))
    "the faces used for the quote of verses")

(defface my&welcome-screen-action
    '((((background light)) :foreground "#0398fc")
      (((background dark)) :foreground "#73915e"))
    "the faces used for the actions on the welcome screen")

(define-button-type 'my&welcome-screen-action-button
    'face 'my&welcome-screen-action)

(defun my:welcome-screen-make-text-button (start end action-string)
    "Make a button with ACTION-STRING as its label."
    (let ((action (alist-get action-string my$actions)))
        (make-text-button
         start end
         'type 'my&welcome-screen-action-button
         'action (lambda (_)
                     (if (commandp action)
                             (call-interactively action)
                         (funcall action))))))

(defun my:generate-initial-messages ()
    (let ((head-verse (nth (random (length my$header-verses))
                           my$header-verses))
          (foot-verse (nth (random (length my$foot-verses))
                           my$foot-verses))
          (action-strings (mapcar #'car my$actions))
          (empty-lines (cl-loop
                        for i from 1 to my$empty-lines-between-sections
                        concat "\n")))
        (setq head-verse (mapconcat #'my:center-a-line head-verse "\n"))
        (setq action-strings (mapconcat #'my:center-a-line action-strings "\n"))
        (setq foot-verse (mapconcat #'my:center-a-line foot-verse "\n"))
        (concat head-verse empty-lines action-strings empty-lines foot-verse)))

(defun my:generate-button-with-actions ()
    (let ((action-strings (mapcar #'car my$actions)))
        (dolist (action-string action-strings)
            ;; find the position of the action string
            (goto-char (point-min))
            (search-forward action-string)
            (let ((start (match-beginning 0)) ;; 0 means the whole match
                  (end (match-end 0)))
                (my:welcome-screen-make-text-button start end action-string)))))


(defun my:center-a-line (x)
    "center one line of verse or action string"
    (let ((spaces-to-be-inserted
           (/ (- my$right-margin-when-centering-margin (string-width x))
              2)))
        (concat (cl-loop for i from 1 to spaces-to-be-inserted concat " ")
                x)))

(defun my:verses-add-font-lock ()
    (font-lock-add-keywords
     nil
     '(("^ +\\([^\"]+\\)$" 1 'my&verses)
       ("^ +\\(.+\\)$" 1 'my&verse-quotes))))

;;;###autoload
(defun my:show-verses-at-startup ()
    (setq initial-scratch-message (my:generate-initial-messages))
    (add-hook 'emacs-startup-hook #'my:verses-add-font-lock)
    (add-hook 'emacs-startup-hook #'my:generate-button-with-actions)
    (add-hook 'emacs-startup-hook (my/turn-off-mode display-line-numbers-mode))
    (add-hook 'emacs-startup-hook (my/setq-locally mode-line-format nil)))

;;;###autoload
(defun my~refresh-verses ()
    "refresh verses in the scratch buffer"
    (interactive)
    (with-current-buffer "*scratch*"
        (erase-buffer)
        (insert (my:generate-initial-messages))
        (my:generate-button-with-actions)))

(defvar my$tab-bar-tab-name-open "")
(defvar my$tab-bar-tab-name-close "")
(defvar my$tab-bar-group-name-open " ")
(defvar my$tab-bar-group-name-close " ")

(defun my:tab-bar-tab-name-format (tab i)
    "This is a slightly modified version of
`tab-bar-tab-name-format-default', which is the default value of
`tab-bar-tab-name-format', except that it adds two symbols indicating
the tab more distinguisably."
    (let ((current-p (eq (car tab) 'current-tab)))
        (propertize
         (concat my$tab-bar-tab-name-open
                 (if tab-bar-tab-hints (format "%d " i) "")
                 (alist-get 'name tab)
                 (or (and tab-bar-close-button-show
                          (not (eq tab-bar-close-button-show
                                   (if current-p 'non-selected 'selected)))
                          tab-bar-close-button)
                     "")
                 my$tab-bar-tab-name-close)
         'face (funcall tab-bar-tab-face-function tab))))

(defun my:tab-bar-tab-group-format (tab i &optional current-p)
    "This is a slightly modified version of
`tab-bar-tab-group-format-default', which is the default value of
`tab-bar-tab-group-format', except that it adds two symbols indicating
the group more distinguisably."
    (propertize
     (concat my$tab-bar-group-name-open
             (if tab-bar-tab-hints (format "%d " i) "")
             (funcall tab-bar-tab-group-function tab)
             my$tab-bar-group-name-close)
     'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))

(provide 'my-ui-autoloads)
;;; my-ui-autoloads ends here
