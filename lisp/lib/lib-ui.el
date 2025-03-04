;;; lib-ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mg-display-truncation-and-wrap-indicator-as-whitespace ()
    (when (not (char-table-p buffer-display-table))
        (setq buffer-display-table (make-display-table)))
    (set-display-table-slot buffer-display-table 'truncation 32)
    (set-display-table-slot buffer-display-table 'wrap 32))

;;;###autoload
(defmacro mg-tab-bar-go-to-tab-macro (number)
    (let ((fun (intern (format "mg-tab-bar-go-to-tab-%d" number))))
        `(defun ,fun ()
             ,(format "go to tab %d" number)
             (interactive)
             (tab-bar-select-tab ,number))))

;;;###autoload
(defun mg-set-scratch-directory (old-fun &rest args)
    "After creating a new tab, the default buffer to be displayed is
scratch buffer whose directory is set to where emacs is initialized.
Change it to the directory of previous buffer where `tab-bar-new-tab'
is called."
    (let ((current-dir default-directory))
        (apply old-fun args)
        (setq-local default-directory current-dir)))

(defun mg--tab-bar-new-buffer ()
    (get-buffer-create "*scratch*"))

;;;###autoload
(defun mg--font-set-small-mono-font ()
    "Set the default font to a smaller sized font for current buffer."
    (face-remap-add-relative 'default :height 140))

;;;###autoload
(defun mg--font-set-small-variable-font ()
    "Set the default font to a smaller sized font for current buffer."
    (face-remap-add-relative 'default :height 140 :family "Bookerly"))

(defvar mg-header-verses
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

(defvar mg-foot-verses
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

(defvar mg-actions
    '((" New [T]heme      " . load-theme)
      (" New [V]erse      " . mg-refresh-verses)
      (" [S]tartup Time   " . mg-emacs-startup-time)
      (" Org [A]genda     " . org-agenda-list)
      (" Recent [F]iles   " . consult-recent-file)
      (" Recent [P]rojects" . project-switch-project))
    "the actions to be displayed on the welcome screen")

(defun mg--empty-lines-between-sections ()
    (let ((height (window-height)))
        (cond ((< height 40) 3)
              ((< height 50) 8)
              (t 10))))

(defun mg--top-lines-padding (content-length)
    (let* ((height (window-height))
           (paddings (ceiling (* (- height content-length) 0.5)))
           (paddings (max 0 paddings)))
        (cl-loop for i from 1 to paddings concat "\n")))

(defun mg--right-margin-when-centering-margin ()
    "The absolute symmetry is in fact less aesthetically pleasing than a slight leftward skew."
    (let ((width (window-width)))
        (cond ((< width 100) width)
              ((< width 200) (- width 10))
              (t (- width 15)))))

(defun mg-emacs-startup-time ()
    "measure the startup time until the welcome screen is displayed. More accurate than `emacs-init-time'"
    (interactive)
    (message (format "%f seconds"
                     (max (float-time (time-subtract mg--welcome-screen-setup-time before-init-time))
                          (float-time (time-subtract after-init-time before-init-time))))))

(defun mg--welcome-screen-set-keymap ()
    (let* ((action-strings (mapcar #'car mg-actions))
           (actions (mapcar #'cdr mg-actions))
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

(defface mg-verses
    '((((background light)) :foreground "#ed80b5" :slant italic)
      (((background dark)) :foreground "#a070b5" :slant italic))
    "the faces used for the verses")

(defface mg-verse-quotes
    '((((background light)) :foreground "#e8ae92")
      (((background dark)) :foreground "#ad7f2a"))
    "the faces used for the quote of verses")

(defface mg-welcome-screen-action
    '((((background light)) :foreground "#0398fc")
      (((background dark)) :foreground "#73915e"))
    "the faces used for the actions on the welcome screen")

(defface mg-welcome-screen-action-key
    '((((background light)) :foreground "#6b82e2")
      (((background dark)) :foreground "#cfe8a3"))
    "the faces used for the actions on the welcome screen")

(defun mg--generate-initial-messages ()
    (let* ((head-verse (nth (random (length mg-header-verses))
                            mg-header-verses))
           (foot-verse (nth (random (length mg-foot-verses))
                            mg-foot-verses))
           (action-strings (mapcar #'car mg-actions))
           (n-lines-between-section (mg--empty-lines-between-sections))
           ;; require one additionl \n inserted at the end of other sections.
           (lines-between-sections
            (cl-loop for i from 1 to (1+ n-lines-between-section) concat "\n"))
           (content-length (+ (length head-verse)
                              (length foot-verse)
                              (length action-strings)
                              (* 2 n-lines-between-section)))
           (top-paddings (mg--top-lines-padding content-length)))
        (setq head-verse (mapconcat #'mg--center-a-line head-verse "\n"))
        (setq action-strings (mapconcat #'mg--center-a-line action-strings "\n"))
        (setq foot-verse (mapconcat #'mg--center-a-line foot-verse "\n"))
        (concat top-paddings
                head-verse
                lines-between-sections
                action-strings
                lines-between-sections
                foot-verse)))

(defun mg--center-a-line (x)
    "center one line of verse or action string"
    (let ((spaces-to-be-inserted
           (/ (- (mg--right-margin-when-centering-margin) (string-width x))
              2)))
        (concat (cl-loop for i from 1 to spaces-to-be-inserted concat " ")
                x)))

(defun mg--verses-add-font-lock ()
    (font-lock-add-keywords
     nil
     '(("^ *\\([^\"]+\\)$" 1 'mg-verses)
       ("^ *\\(.+\\)$" 1 'mg-verse-quotes)
       ("^ *\\(.+\\)\\[" 1 'mg-welcome-screen-action)
       ("\\(\\[.*\\]\\)" 1 'mg-welcome-screen-action-key)
       ("\\]\\(.*\\)$" 1 'mg-welcome-screen-action))))

;;;###autoload
(defun mg--welcome-screen-mode ()
    (setq initial-scratch-message nil)
    (add-hook 'emacs-startup-hook #'mg--set-welcome-screen-buffer)
    ;; when running emacs in server mode, cannot get the window
    ;; height/width at startup
    (add-hook 'server-after-make-frame-hook #'mg-refresh-verses))

(defun mg--set-welcome-screen-buffer ()
    (with-current-buffer "*scratch*"
        (insert (mg--generate-initial-messages))
        (mg--verses-add-font-lock)
        (setq-local mode-line-format nil)
        (mg--welcome-screen-set-keymap))
    (setq mg--welcome-screen-setup-time (current-time)))

;;;###autoload
(defun mg-refresh-verses ()
    "refresh verses in the scratch buffer"
    (interactive)
    (with-current-buffer "*scratch*"
        (erase-buffer)
        (insert (mg--generate-initial-messages))))

(defvar mg-tab-bar-tab-name-open "")
(defvar mg-tab-bar-tab-name-close "")
(defvar mg-tab-bar-group-name-open " ")
(defvar mg-tab-bar-group-name-close " ")

(defun mg--tab-bar-add-custom-boundaries (name _ _)
    "Add custom separators around tab names in the tab-bar.
Unlike `tab-bar-separator' which uses identical symbols for both sides,
this function applies different symbols defined by
`mg-tab-bar-tab-name-open' and `mg-tab-bar-tab-name-close' as
boundaries."
    (concat mg-tab-bar-tab-name-open
            name
            mg-tab-bar-tab-name-close))

(defun mg--tab-bar-tab-group-format (tab i &optional current-p)
    "This is a slightly modified version of
`tab-bar-tab-group-format-default', which is the default value of
`tab-bar-tab-group-format', except that it adds two symbols indicating
the group more distinguisably."
    (propertize
     (concat mg-tab-bar-group-name-open
             (if tab-bar-tab-hints (format "%d " i) "")
             (funcall tab-bar-tab-group-function tab)
             mg-tab-bar-group-name-close)
     'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))

(defun mg--get-tab-name (buffer alist)
    "Retrieve the name of a tab associated with a BUFFER.  This
function is intended for use with `display-buffer-in-tab'.  The
behavior is straightforward: if a tab already exists with the same
name as the BUFFER, it is reused; otherwise, a new tab is created."
    (buffer-name buffer))

(provide 'lib-ui)
;;; lib-ui ends here
