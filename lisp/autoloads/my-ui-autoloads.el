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

(defvar my$empty-lines-between-header-and-foot-verse 20)

(defvar my$right-margin-when-centering-margin 80
    "The assumed window width to calculate appropriate number of
whitespaces to be prepended when centering the verses.")

(defface my&verses '((t :foreground "#a070b5" :slant italic)) "the faces used for the verses")

(defface my&verse-quotes '((t :foreground "#ad7f2a")) "the faces used for the quote of verses")

(defun my:generate-initial-messages ()
    (let ((head-verse (nth (random (length my$header-verses))
                           my$header-verses))
          (foot-verse (nth (random (length my$foot-verses))
                           my$foot-verses))
          (empty-lines (cl-loop
                        for i from 1 to my$empty-lines-between-header-and-foot-verse
                        concat "\n")))
        (setq head-verse (mapconcat #'my:center-verse head-verse "\n"))
        (setq foot-verse (mapconcat #'my:center-verse foot-verse "\n"))
        (concat head-verse empty-lines foot-verse)))

(defun my:center-verse (x)
    "center one line of verse"
    (let ((spaces-to-be-inserted
           (/ (- my$right-margin-when-centering-margin (length x))
              2)))
        (concat (cl-loop for i from 1 to spaces-to-be-inserted concat " ")
                x)))

(defun my:verses-add-font-lock ()
    (with-current-buffer (current-buffer)
            (font-lock-add-keywords
             nil
             '(("^ +\\([^\"]+\\)$" 1 'my&verses)
               ("^ +\\(.+\\)$" 1 'my&verse-quotes)))))

;;;###autoload
(define-minor-mode my~show-verses-at-startup-mode
    "show verses at the startup screen."
    :global t

    (setq initial-scratch-message (my:generate-initial-messages))
    (add-hook 'emacs-startup-hook #'my:verses-add-font-lock))

(provide 'my-ui-autoloads)
;;; my-ui-autoloads ends here
