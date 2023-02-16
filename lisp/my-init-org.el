;;; my-init-org.el -*- lexical-binding: t; -*-

(straight-use-package '(org :type built-in))
(straight-use-package 'evil-org)
(straight-use-package 'org-appear)
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
          ;; NOTE: inline image link cannot have description.  See org
          ;; manual 12.7 for further instruction if you want to add
          ;; description for a link which is inline image.  By default
          ;; the "remote" means tramp files. Inline http image is not
          ;; supported yet.
          org-display-remote-inline-images 'download
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
          org-hide-emphasis-markers t
          org-pretty-entities t ;; display \alpha as utf8 chars in overlay
          org-image-actual-width nil
          org-imenu-depth 6
          ;; Save target buffer after archiving a node.
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
                                        "|" "DONE(d)" "KILL(k)"))
          ;; don't load those org modules that I never use
          org-modules '(ol-doi ol-w3m ol-bbdb ol-bibtex ol-info ol-eww))

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

    :config
    (evil-set-initial-state 'org-agenda-mode 'motion)
    (add-to-list 'org-file-apps '(remote . emacs))
    ;; turn off evil-vimish, use org builtin fold
    (add-hook 'org-mode-hook #'my/turn-off-evil-vimish)
    (add-hook 'org-tab-first-hook #'my/org-indent-maybe-h)
    (add-hook 'org-tab-first-hook #'my/org-yas-expand-maybe-h)

    (general-define-key
     :states 'normal
     :keymaps 'org-mode-map
     "TAB" #'org-cycle
     "RET" #'evil-org-return
     ;; RET is overridden by `er/expand-region'
     [remap imenu] #'consult-org-heading
     [remap consult-imenu] #'consult-org-heading)

    ;; copied from doom emacs
    (my/localleader
        :states '(normal insert motion visual)
        :keymaps 'org-mode-map
        "RET" #'er/expand-region
        "#" #'org-update-statistics-cookies
        "'" #'org-edit-special
        "*" #'org-ctrl-c-star
        "+" #'org-ctrl-c-minus
        "," #'org-switchb
        "." #'org-goto
        "@" #'org-cite-insert
        "." #'consult-org-heading
        "/" #'consult-org-agenda
        "A" #'org-archive-subtree
        "e" #'org-export-dispatch
        "f" #'org-footnote-action
        "h" #'org-toggle-heading
        "i" #'org-toggle-item
        "I" #'org-id-get-create
        "k" #'org-babel-remove-result
        "n" #'org-store-link
        "o" #'org-set-property
        "q" #'org-set-tags-command
        "t" #'org-todo
        "T" #'org-todo-list
        "x" #'org-toggle-checkbox

        "a" '(:ignore t :which-key "attach")
        "aa" #'org-attach
        "ad" #'org-attach-delete-one
        "aD" #'org-attach-delete-all
        "an" #'org-attach-new
        "ao" #'org-attach-open
        "aO" #'org-attach-open-in-emacs
        "ar" #'org-attach-reveal
        "aR" #'org-attach-reveal-in-emacs
        "au" #'org-attach-url
        "as" #'org-attach-set-directory
        "aS" #'org-attach-sync

        "b" '(:ignore t :which-key "table")
        "b-" #'org-table-insert-hline
        "ba" #'org-table-align
        "bb" #'org-table-blank-field
        "bc" #'org-table-create-or-convert-from-region
        "be" #'org-table-edit-field
        "bf" #'org-table-edit-formulas
        "bh" #'org-table-field-info
        "bs" #'org-table-sort-lines
        "br" #'org-table-recalculate
        "bR" #'org-table-recalculate-buffer-tables

        "bd" '(:ignore t :which-key "delete")
        "bdc" #'org-table-delete-column
        "bdr" #'org-table-kill-row

        "bi" '(:ignore t :which-key "insert")
        "bic" #'org-table-insert-column
        "bih" #'org-table-insert-hline
        "bir" #'org-table-insert-row
        "biH" #'org-table-hline-and-move

        "bt" '(:ignore t :which-key "toggle")
        "btf" #'org-table-toggle-formula-debugger
        "bto" #'org-table-toggle-coordinate-overlays

        "c" '(:ignore t :which-key "clock")
        "cc" #'org-clock-cancel
        "cd" #'org-clock-mark-default-task
        "ce" #'org-clock-modify-effort-estimate
        "cE" #'org-set-effort
        "cg" #'org-clock-goto
        "ci" #'org-clock-in
        "cI" #'org-clock-in-last
        "co" #'org-clock-out
        "cr" #'org-resolve-clocks
        "cR" #'org-clock-report
        "ct" #'org-evaluate-time-range
        "c=" #'org-clock-timestamps-up
        "c-" #'org-clock-timestamps-down

        "d" '(:ignore t :which-key "date/ddl")
        "dd" #'org-deadline
        "ds" #'org-schedule
        "dt" #'org-time-stamp
        "dT" #'org-time-stamp-inactive

        "g" '(:ignore t :which-key "goto")
        "gg" #'consult-org-heading
        "gG" #'consult-org-agenda
        "gc" #'org-clock-goto
        "gi" #'org-id-goto
        "gr" #'org-refile-goto-last-stored
        "gx" #'org-capture-goto-last-stored

        "l" '(:ignore t :which-key "link")
        "lc" #'org-cliplink ;; TODO: require org-cliplink
        "li" #'org-id-store-link
        "ll" #'org-insert-link
        "lL" #'org-insert-all-links
        "ls" #'org-store-link
        "lS" #'org-insert-last-stored-link
        "lt" #'org-toggle-link-display
        "lg" #'org-mac-link-get-link ;; TODO: require org-mac-link

        "p" '(:ignore t :which-key "priority")
        "pd" #'org-priority-down
        "pp" #'org-priority
        "pu" #'org-priority-up

        "P" '(:ignore t :which-key "publish")
        "Pa" #'org-publish-all
        "Pf" #'org-publish-current-file
        "Pp" #'org-publish
        "PP" #'org-publish-current-project
        "Ps" #'org-publish-sitemap

        "r" '(:ignore t :which-key "refile")
        "rr" #'org-refile
        "rR" #'org-refile-reverse ; to all `org-refile-targets'

        "s" '(:ignore t :which-key "subtree")
        "sa" #'org-toggle-archive-tag
        "sb" #'org-tree-to-indirect-buffer
        "sc" #'org-clone-subtree-with-time-shift
        "sd" #'org-cut-subtree
        "sh" #'org-promote-subtree
        "sj" #'org-move-subtree-down
        "sk" #'org-move-subtree-up
        "sl" #'org-demote-subtree
        "sn" #'org-narrow-to-subtree
        "sr" #'org-refile
        "ss" #'org-sparse-tree
        "sA" #'org-archive-subtree
        "sN" #'widen
        "sS" #'org-sort

        )

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

    (advice-add #'org-get-agenda-file-buffer :around #'my/exclude-org-agenda-buffers-from-recentf)
    (add-hook 'org-agenda-finalize-hook #'my/reload-org-agenda-buffers)

    (my/localleader
        :states '(normal insert visual motion)
        :keymaps 'org-agenda-mode-map
        "d" '(:ignore t :which-key "ddl")
        "dd" #'org-agenda-deadline
        "ds" #'org-agenda-schedule

        "c" '(:ignore t :which-key "clock")
        "cc" #'org-agenda-clock-cancel
        "cg" #'org-agenda-clock-goto
        "ci" #'org-agenda-clock-in
        "co" #'org-agenda-clock-out
        "cr" #'org-agenda-clockreport-mode
        "cs" #'org-agenda-show-clocking-issues

        "p" '(:ignore t :which-key "priority")
        "pd" #'org-agenda-priority-down
        "pp" #'org-agenda-priority
        "pu" #'org-agenda-priority-up

        "T" #'org-agenda-set-tags
        "r" #'org-agenda-refile
        "t" #'org-agenda-todo

        )

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
          org-src-window-setup 'plain)

    :config
    (add-to-list 'org-src-lang-modes '("r" . R))
    (add-to-list 'display-buffer-alist
                 '("\\*Org Src"
                   (display-buffer-at-bottom)
                   (window-height . 0.8)))
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
    (my/org-babel-lsp-setup "R")
    (my/org-babel-lsp-setup "python")

    ;; TODO: update `org-babel-python-command' in accordance to `python-shell-interpreter'
    ;; and `python-shell-interpreter-args'

    )

(use-package org-attach
    :init
    (setq org-attach-store-link-p t     ; store link after attaching files
          org-attach-use-inheritance t ; inherit properties from parent nodes
          ;; Centralized attachments directory by default
          org-attach-id-dir (expand-file-name ".attach/" org-directory))

    ;; Autoload all these commands that org-attach doesn't autoload itself
    :commands (org-attach-new
               org-attach-open
               org-attach-open-in-emacs
               org-attach-reveal-in-emacs
               org-attach-url
               org-attach-set-directory
               org-attach-sync)
    )

(use-package ol
    :init
    (setq org-link-abbrev-alist
          '(("github"      . "https://github.com/%s")
            ("youtube"     . "https://youtube.com/watch?v=%s")
            ("google"      . "https://google.com/search?q=")
            ("gimages"     . "https://google.com/images?q=%s")
            ("gmap"        . "https://maps.google.com/maps?q=%s")
            ("duckduckgo"  . "https://duckduckgo.com/?q=%s")
            ("wikipedia"   . "https://en.wikipedia.org/wiki/%s")
            ("wolfram"     . "https://wolframalpha.com/input/?i=%s"))

          org-link-file-path-type 'relative)
    )

(use-package ox
    :init
    ;; referenced from doomemacs, I haven't fully understand the
    ;; rationale behind it yet.
    (setq org-export-with-smart-quotes t
          org-html-validation-link nil
          org-latex-prefer-user-labels t)

    :config
    (add-to-list 'org-export-backends 'md))

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

;; make org fontification works similar to vim's conceal.
;; moving point over concealed text reveals its content.
(use-package org-appear
    :hook (org-mode . org-appear-mode)
    :init
    (setq org-appear-autolinks t
          org-appear-autoentities t)

    (when (display-graphic-p)
        (setq org-appear-autosubmarkers t)))

(provide 'my-init-org)
;;; my-init-org.el ends here
