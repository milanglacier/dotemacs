;;; my-init-org.el -*- lexical-binding: t; -*-

(straight-use-package 'evil-org)
;; (straight-use-package 'org-contrib)
;; (straight-use-package 'htmlize)
;; (straight-use-package 'jupyter)
;; (straight-use-package 'org-cliplink)
;; (straight-use-package 'orgit)
;; (straight-use-package 'ob-async)
;; (straight-use-package 'org-re-reveal)
;; (straight-use-package 'org-pdftools)
;; (straight-use-package 'org-cliplink)
;; (straight-use-package 'toc-org)
;; (straight-use-package 'ox-clip)
;; (straight-use-package 'avy)

;; (when IS-MAC
;;   (straight-use-package 'org-mac-link))

(defun my/general-org-capture-template (letter desc &rest args)
    "a wrapper to generate my frequently used `org-capture' template.
args is a `plist' which accpets following keys:
`:headings', `:target-head', `:propertis',`:file', `:template', `:type'.
`:headings' is a list of headings (of type string),
`:type' is the type of an entry, by default it is `entry',
`:target-head' is first element of target,
by default it is `file+olp+datetree',
`:properties' should be a list,
the element of which should match the properties of
`org-capture-templates',
by default it contains `:prepend t',
`:file' should be a symbol whose value is the file
`:template' should be a string indicating the template to be filled"
    (let* ((file (plist-get args :file))
           (type (or (plist-get args :type) 'entry))
           (headings (plist-get args :headings))
           (properties (append (plist-get args :properties)
                               '(:prepend t)))
           (target-head (or (plist-get args :target-head) 'file+olp+datetree))
           (target (append `(,target-head ,file) headings))
           (template (plist-get args :template)))
        (append `(,letter ,desc ,type)
                `(,target)
                `(,template)
                properties)))

(defun my/org-capture-bubble-tea-template (letter desc headings template &rest properties)
    `(,letter ,desc table-line
              (file+olp ,my/org-capture-bubble-tea-live-file
                        ,@headings ,(format-time-string "%Y") ,(format-time-string "%B"))
              ,template ,@properties))

(defun my/org-agenda-visited-all-directories ()
    "Org agenda need to visted all files listed in `org-agenda-files'
to create the view, which is expensive. By default I will only list a
small portion of files to be searched.  This function searches all the
files in the org-directory to create the org-agenda view"
    (interactive)
    (let* ((default-directory org-directory)
           (directories-string (shell-command-to-string "find * -type d -print0"))
           ;; the literal null character will cause git to wrongly
           ;; consider this file as a binary file. Use the `kbd' to
           ;; get the internal representation of the null character
           (directories (split-string directories-string (kbd "^@") nil))
           (org-agenda-files (mapcar (lambda (x)
                                         (file-name-concat org-directory x))
                                     directories)))
        (call-interactively #'org-agenda)))

(use-package org
    :ensure nil
    :defer t
    :init

    (my/open-map
        :states '(normal motion visual insert)
        :keymaps 'override
        "a" #'org-agenda
        "A" #'my/org-agenda-visited-all-directories
        "c" #'org-capture
        "o" #'org-clock-goto)

    (setq org-directory "~/Desktop/orgmode"
          org-archive-location (expand-file-name "%s_archive::" (concat org-directory "/archive"))
          org-id-locations-file (file-name-concat org-directory ".orgids")
          org-id-locations-file-relative t
          org-highlight-latex-and-related '(native latex script entities)
          org-special-ctrl-a/e t
          org-indirect-buffer-display 'current-window
          org-tags-column 0 ;; don't indent tags, put tags directly behind the heading
          org-M-RET-may-split-line nil
          org-insert-heading-respect-content t
          org-startup-indented t
          org-enforce-todo-dependencies t
          org-fontify-done-headline t
          org-fontify-quote-and-verse-blocks t
          org-fontify-whole-heading-line t
          org-hide-leading-stars t
          org-image-actual-width nil
          org-imenu-depth 6
          org-archive-subtree-save-file-p t
          org-priority-faces '((?A . error)
                               (?B . warning)
                               (?C . success))
          org-startup-indented t
          org-tags-column 0
          org-use-sub-superscripts '{}
          ;; `showeverything' is org's default, but it doesn't respect
          ;; `org-hide-block-startup' (#+startup: hideblocks), archive trees,
          ;; hidden drawers, or VISIBILITY properties. `nil' is equivalent, but
          ;; respects these settings.
          org-startup-folded nil
          ;; Prevent modifications made in invisible sections of an org document, as
          ;; unintended changes can easily go unseen otherwise.
          org-catch-invisible-edits 'smart
          org-preview-latex-image-directory (file-name-concat "~/.cache" "ltximg/")
          org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "WAIT(w)" "HOLD(h)"
                                        "|" "DONE(d)" "KILL(k)")))

    ;; copied from doomemacs
    (setq org-refile-targets
          '((nil :maxlevel . 5)
            (org-agenda-files :maxlevel . 5))
          ;; Without this, completers like ivy/helm are only given the first level of
          ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
          ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
          ;; target! e.g. FILE/Tasks/heading/subheading
          org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil)

    (with-no-warnings
        (custom-declare-face 'my/org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
        (custom-declare-face 'my/org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
        (custom-declare-face 'my/org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

    (setq org-todo-keyword-faces '(("STRT" . my/org-todo-active)
                                   ("WAIT" . my/org-todo-onhold)
                                   ("HOLD" . my/org-todo-onhold)
                                   ("KILL" . my/org-todo-cancel)))

    ;; turn off evil-vimish, use org builtin fold
    (add-hook 'org-mode-hook (defun my/turn-off-evil-vimish () (evil-vimish-fold-mode -1)))

    :config
    (evil-set-initial-state 'org-agenda-mode 'motion)

    (general-define-key
     :states 'normal
     :keymaps 'org-mode-map
     "TAB" #'org-cycle
     [remap imenu] #'consult-org-heading
     [remap consult-imenu] #'consult-org-heading)

    (my/localleader
        :states '(normal insert)
        :keymaps 'org-mode-map
        "t" #'org-todo
        "x" #'org-toggle-checkbox
        "c" '(:ignore t :which-key "clock")
        "co" #'org-clock-out
        "ci" #'org-clock-in
        "cc" #'org-clock-cancel)

    )

(use-package org-capture
    :ensure nil
    :defer t

    :config
    (setq my/org-capture-todo-file (file-name-concat "capture" "todo.org")
          my/org-capture-notes-file (file-name-concat "capture" "notes.org")
          my/org-capture-english-note-file (file-name-concat "capture" "english.org")
          my/org-capture-bubble-tea-live-file (file-name-concat "capture" "bubble-tea.org"))

    (setq org-capture-templates
          `(
            ,(my/general-org-capture-template
              "t" "Personal todo"
              :target-head 'file
              :file my/org-capture-todo-file
              :template "* TODO %?%^g\nSCHEDULED: %t")
            ,(my/general-org-capture-template
              "n" "Personal notes"
              :target-head 'file :file my/org-capture-notes-file
              :template "* %u %?\n%i\n%a")
            ,(my/general-org-capture-template
              "j" "Applied jobs"
              :type 'checkitem :file my/org-capture-todo-file
              :headings '("Applied jobs")
              :template "- [ ] %?")
            ,(my/general-org-capture-template
              "i" "clock in(start a timer)"
              :target-head 'file :file my/org-capture-todo-file
              :template "* TODO %?%^g\nSCHEDULED: %t"
              :properties '(:clock-in t :clock-keep t))

            ("e" "English notes")
            ,(my/general-org-capture-template
              "em" "Manually fill"
              :file my/org-capture-english-note-file
              :template "* %^{prompt}\n%i\n\n%a\n:explanation:\n%?\n:END:")
            ,(my/general-org-capture-template
              "ew" "quick fill with word and sentence under point"
              :file my/org-capture-english-note-file
              :template (concat "* %(with-current-buffer (org-capture-get :original-buffer) (current-word))"
                                "\n%(with-current-buffer (org-capture-get :original-buffer) (thing-at-point 'sentence t))"
                                "\n\n%a\n:explanation:\n%?\n:END:"))
            ,(my/general-org-capture-template
              "er" "quick fill with selected region and sentence under point"
              :file my/org-capture-english-note-file
              :template (concat "* %i"
                                "\n%(with-current-buffer (org-capture-get :original-buffer) (thing-at-point 'sentence t))"
                                "\n\n%a\n:explanation:\n%?\n:END:"))

            ("b" "bubble tea")
            ,(my/org-capture-bubble-tea-template "bf" "feed food" '("feed food") "|%U|Taste%?|||||")
            ,(my/org-capture-bubble-tea-template "bp" "poop" '("poop") "|%U|Indoor%?|||||")
            ,(my/org-capture-bubble-tea-template "bP" "play" '("play") "|%U|Street Walk%?|||||"
                                                 :clock-in t :clock-keep t)
            ;; TODO: examine the usage of clock-table
            ,(my/org-capture-bubble-tea-template "be" "eye mucus" '("clean" "eye mucus") "|%U%?||")
            ,(my/org-capture-bubble-tea-template "bE" "ear clean" '("clean" "ear clean") "|%U%?||")
            ,(my/org-capture-bubble-tea-template "bb" "bath" '("clean" "bath") "|%U%?||")
            ,(my/org-capture-bubble-tea-template "bt" "trim coat" '("clean" "trim coat") "|%U%?||")
            ,(my/org-capture-bubble-tea-template "bg" "grooming" '("clean" "grooming") "|%U%?||")
            ,(my/org-capture-bubble-tea-template "bn" "nailing" '("clean" "nailing") "|%U%?||")
            ,(my/org-capture-bubble-tea-template "bB" "brushing teeth" '("clean" "brushing teeth") "|%U%?||")
            ,(my/org-capture-bubble-tea-template "bs" "symptom" '("symptom") "|%U|Vomit%?|||")

            ;; TODO: add doomemacs's project org capture template.
            ))
    (org-capture-put :kill-buffer t) ;; kill org capture buffer by default
    ;; when refiling from org-capture, Emacs prompts to kill the
    ;; underlying, modified buffer. This fixes that.
    (add-hook 'org-after-refile-insert-hook #'save-buffer)

    )

(use-package org-agenda
    :ensure nil
    :defer t

    :init
    ;; Different colors for different priority levels
    (setq org-agenda-deadline-faces '((1.001 . error)
                                      (1.0 . org-warning)
                                      (0.5 . org-upcoming-deadline)
                                      (0.0 . org-upcoming-distant-deadline))
          org-agenda-window-setup 'current-window
          org-agenda-skip-unavailable-files t
          org-agenda-span 10
          ;; always start on today
          org-agenda-start-on-weekday nil
          org-agenda-start-day "-3d"
          org-agenda-inhibit-startup t
          org-agenda-window-setup 'other-tab)

    :config

    ;; org-agenda will visit all org files listed
    ;; in `org-agenda-files' to generate the org-agenda view.
    ;; avoid too much files inside this directory.
    (setq  org-agenda-files `(,org-directory
                              ,@(mapcar
                                 (lambda (x) (file-name-concat org-directory x))
                                 '("capture"))))

    )

(use-package ob
    :ensure nil
    :defer t
    :init
    (setq org-src-preserve-indentation t
          org-edit-src-content-indentation 0
          ;; org-src-tab-acts-natively t
          org-confirm-babel-evaluate nil
          org-link-elisp-confirm-function nil
          ;; Show src buffer in popup, and don't monopolize the frame
          org-src-window-setup 'other-window))

(use-package evil-org
    :hook (org-mode . evil-org-mode)

    :init
    (add-hook 'org-capture-mode-hook #'evil-insert-state)

    :config
    (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
    (evil-org-set-key-theme))

(use-package evil-org-agenda
    :after org-agenda
    :config
    (evil-org-agenda-set-keys))

(provide 'my-init-org)
;;; my/init-ui.el ends here
