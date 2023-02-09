;;; my-init-org.el -*- lexical-binding: t; -*-

(straight-use-package '(org :type built-in))
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

(use-package org
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
          org-return-follows-link t
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
     "RET" #'evil-org-return
     ;; RET is overridden by `er/expand-region'
     [remap imenu] #'consult-org-heading
     [remap consult-imenu] #'consult-org-heading)

    (my/localleader
        :states '(normal insert)
        :keymaps 'org-mode-map
        "RET" #'er/expand-region
        "t" #'org-todo
        "x" #'org-toggle-checkbox
        "c" '(:ignore t :which-key "clock")
        "co" #'org-clock-out
        "ci" #'org-clock-in
        "cc" #'org-clock-cancel)

    (run-with-idle-timer 2 nil #'my/load-org-extensions-idly)

    )

(use-package org-capture
    :config
    (setq my/org-capture-todo-file (file-name-concat "capture" "todo.org")
          my/org-capture-notes-file (file-name-concat "capture" "notes.org")
          my/org-capture-english-note-file (file-name-concat "capture" "english.org")
          my/org-capture-bubble-tea-live-file (file-name-concat "capture" "bubble-tea.org"))

    (setq org-capture-templates
          `(("t" "Personal todo" entry
             (file ,my/org-capture-todo-file)
             "* TODO %?%^g\nSCHEDULED: %t" :prepend t)
            ("n" "Personal notes" entry
             (file ,my/org-capture-notes-file)
             "* %u %?\n%i\n%a" :prepend t)
            ("j" "Applied jobs" checkitem
             (file+olp ,my/org-capture-todo-file "Applied jobs")
             "- [ ] %?")
            ("i" "clock in(start a timer)" entry
             (file ,my/org-capture-todo-file)
             "* TODO %?%^g\nSCHEDULED: %t"
             :clock-in t :clock-keep t :prepend t)

            ("e" "English notes")
            ("em" "Manually fill" entry
             (file+olp+datetree ,my/org-capture-english-note-file)
             "* %^{prompt}\n%i\n\n%a\n:explanation:\n%?\n:END:")
            ("ew" "quick fill with word and sentence under point" entry
             (file+olp+datetree ,my/org-capture-english-note-file)
             ,(concat "* %(with-current-buffer (org-capture-get :original-buffer) (current-word))"
                      "\n%(with-current-buffer (org-capture-get :original-buffer) (thing-at-point 'sentence t))"
                      "\n\n%a\n:explanation:\n%?\n:END:"))
            ("er" "quick fill with selected region and sentence under point" entry
             (file+olp+datetree ,my/org-capture-english-note-file)
             ,(concat "* %i"
                      "\n%(with-current-buffer (org-capture-get :original-buffer) (thing-at-point 'sentence t))"
                      "\n\n%a\n:explanation:\n%?\n:END:"))

            ("b" "bubble tea")
            ,(my/org-capture-bubble-tea-template "bf" "feed food" '("feed food") "|%U|Taste%?|||||")
            ,(my/org-capture-bubble-tea-template "bp" "poop" '("poop") "|%U|Indoor%?|||||")
            ,(my/org-capture-bubble-tea-template "bP" "play" '("play") "|%U|Street Walk%?|||||"
                                                 :clock-in t :clock-keep t)
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
    :init
    (setq org-src-preserve-indentation t
          org-edit-src-content-indentation 0
          org-src-tab-acts-natively t
          ;; evil conflicts with this , so this setting has no
          ;; effect.

          ;; TODO: if set `evil-want-C-i-jump' to nil, then
          ;; `org-src-tab-acts-natively' works as expected.  However I
          ;; find that if set `evil-want-C-i-jump' to nil then make
          ;; `better-jumper-mode' becomes incompatible with evil.
          org-confirm-babel-evaluate nil
          org-link-elisp-confirm-function nil
          ;; Show src buffer in popup, and don't monopolize the frame
          org-src-window-setup 'other-window)

    :config
    (add-to-list 'org-src-lang-modes '("r" . R))
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((latex . t)
                                   (R . t)
                                   (emacs-lisp . t)
                                   (shell . t)
                                   (python . t)))

    ;; HACK: when you want to use org-babel to execute a code block,
    ;; it seems that it uses the language identifer associate with
    ;; this block to query the `org-babel-execute:xxx' function.
    (defalias #'org-babel-execute:r #'org-babel-execute:R)

    )

(use-package evil-org
    :hook (org-mode . evil-org-mode)

    :init
    (add-hook 'org-capture-mode-hook #'evil-insert-state)

    :config
    (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
    (evil-org-set-key-theme))

(use-package evil-org-agenda
    :demand t
    :after org-agenda
    :config
    (evil-org-agenda-set-keys))

(provide 'my-init-org)
;;; my-init-org.el ends here
