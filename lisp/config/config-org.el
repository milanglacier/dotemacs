;;; config-org.el -*- lexical-binding: t; -*-

(straight-use-package '(org :type built-in))
(straight-use-package '(evil-org-mode :host github :repo "doomelpa/evil-org-mode"))
(straight-use-package 'org-appear)
(straight-use-package 'jupyter)
(straight-use-package 'org-re-reveal)
(straight-use-package 'ox-clip)
(straight-use-package 'org-download)

(defvar mg-jupyter-want-integration t
    "Enable jupyter integration. which entails configuring it as an
org-babel backend and allowing for direct editing of Jupyter notebooks
within Emacs.")

(use-package org
    :init
    (setq mg-load-incrementally-packages
          (append
           mg-load-incrementally-packages
           '(org-macs org-compat org-faces org-entities
                      org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
                      org-capture)))

    (mg-open-map
        :states '(normal motion visual insert)
        :keymaps 'override
        "a" #'org-agenda
        "A" #'mg-org-agenda-visited-all-directories
        "c" #'org-capture
        "o" #'org-clock-goto)

    (setq org-directory "~/Desktop/orgmode"
          org-archive-location (expand-file-name "%s.archive.org::" (concat org-directory "/archive"))
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
          ;; Resume when clocking into task with open clock
          org-clock-in-resume t
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
          org-modules '(ol-doi ol-bbdb ol-bibtex ol-info ol-eww))

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

    (defface mg-org-todo-active '((t :inherit (bold font-lock-constant-face org-todo))) "")
    (defface mg-org-todo-onhold '((t :inherit (bold warning org-todo))) "")
    (defface mg-org-todo-cancel '((t :inherit (bold error org-todo))) "")

    (setq org-todo-keyword-faces '(("STRT" . mg-org-todo-active)
                                   ("WAIT" . mg-org-todo-onhold)
                                   ("HOLD" . mg-org-todo-onhold)
                                   ("KILL" . mg-org-todo-cancel)))

    :config
    (evil-set-initial-state 'org-agenda-mode 'motion)
    (add-to-list 'org-file-apps '(remote . emacs))
    (add-hook 'org-tab-first-hook #'mg-org-indent-maybe-h)
    (add-hook 'org-tab-first-hook #'mg-org-yas-expand-maybe-h)

    (add-hook 'org-mode-hook (mg--call-func-respect-blocklist eglot-ensure))

    (mg-define-and-bind-local-paren-text-object "/" "/" "/" org-mode-hook)
    (mg-define-and-bind-local-paren-text-object "*" "*" "*" org-mode-hook)

    (general-define-key
     :states 'normal
     :keymaps 'org-mode-map
     "TAB" #'org-cycle
     "RET" #'evil-org-return
     ;; RET is overridden by `er/expand-region'
     [remap imenu] #'consult-org-heading
     [remap consult-imenu] #'consult-org-heading)

    ;; copied from doom emacs
    (mg-localleader
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
        "ac" #'org-download-screenshot
        "aP" #'org-download-clipboard
        "ap" #'org-download-yank

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
        "li" #'org-id-store-link
        "ll" #'org-insert-link
        "lL" #'org-insert-all-links
        "ls" #'org-store-link
        "lS" #'org-insert-last-stored-link
        "lt" #'org-toggle-link-display

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

        "y" #'ox-clip-formatted-copy
        ;; TODO: implement a vim conformant operator.

        )
    )

(use-package org-capture
    :config
    (setq mg-org-capture-todo-file (file-name-concat "capture" "todo.org")
          mg-org-capture-notes-file (file-name-concat "capture" "notes.org")
          mg-org-capture-english-note-file (file-name-concat "capture" "english.org")
          mg-org-capture-bubble-tea-live-file (file-name-concat "capture" "bubble-tea.org"))

    (setq org-capture-templates
          `(("t" "Personal todo" entry
             (file ,mg-org-capture-todo-file)
             "* TODO %?%^g\nSCHEDULED: %t" :prepend t)
            ("n" "Personal notes" entry
             (file ,mg-org-capture-notes-file)
             "* %u %?\n%i\n%a" :prepend t)
            ("i" "clock in(start a timer)" entry
             (file ,mg-org-capture-todo-file)
             "* TODO %?%^g\nSCHEDULED: %t"
             :clock-in t :clock-keep t :prepend t)

            ("e" "English notes")
            ("em" "Manually fill" entry
             (file+olp+datetree ,mg-org-capture-english-note-file)
             "* %^{prompt}\n%i\n\n%a\n:explanation:\n%?\n:END:")
            ("ew" "quick fill with word and sentence under point" entry
             (file+olp+datetree ,mg-org-capture-english-note-file)
             ,(concat "* %(with-current-buffer (org-capture-get :original-buffer) (current-word))"
                      "\n%(with-current-buffer (org-capture-get :original-buffer) (thing-at-point 'sentence t))"
                      "\n\n%a\n:explanation:\n%?\n:END:"))
            ("er" "quick fill with selected region and sentence under point" entry
             (file+olp+datetree ,mg-org-capture-english-note-file)
             ,(concat "* %i"
                      "\n%(with-current-buffer (org-capture-get :original-buffer) (thing-at-point 'sentence t))"
                      "\n\n%a\n:explanation:\n%?\n:END:"))

            ("b" "bubble tea")
            ,(mg-org-capture-bubble-tea-template "bf" "feed food" '("feed food") "|%U|Taste%?|||||")
            ,(mg-org-capture-bubble-tea-template "bp" "poop" '("poop") "|%U|Indoor%?|||||")
            ,(mg-org-capture-bubble-tea-template "bP" "play" '("play") "|%U|Street Walk%?|||||"
                                                 :clock-in t :clock-keep t)
            ,(mg-org-capture-bubble-tea-template "be" "eye mucus" '("clean" "eye mucus") "|%U%?||")
            ,(mg-org-capture-bubble-tea-template "bE" "ear clean" '("clean" "ear clean") "|%U%?||")
            ,(mg-org-capture-bubble-tea-template "bb" "bath" '("clean" "bath") "|%U%?||")
            ,(mg-org-capture-bubble-tea-template "bt" "trim coat" '("clean" "trim coat") "|%U%?||")
            ,(mg-org-capture-bubble-tea-template "bg" "grooming" '("clean" "grooming") "|%U%?||")
            ,(mg-org-capture-bubble-tea-template "bn" "nailing" '("clean" "nailing") "|%U%?||")
            ,(mg-org-capture-bubble-tea-template "bB" "brushing teeth" '("clean" "brushing teeth") "|%U%?||")
            ,(mg-org-capture-bubble-tea-template "bs" "symptom" '("symptom") "|%U|Vomit%?|||")

            ;; TODO: add doomemacs's project org capture template.
            ))

    (org-capture-put :kill-buffer t) ;; kill org capture buffer by default
    ;; when refiling from org-capture, Emacs prompts to kill the
    ;; underlying, modified buffer. This fixes that.
    (add-hook 'org-after-refile-insert-hook #'save-buffer)

    ;; ltex-ls (via eglot) and citre has compatability issue with org-capture-mode
    (add-hook 'org-capture-mode-hook (mg-turn-off-mode flymake-mode))
    (mg-setq-on-hook org-capture-mode-hook
                     completion-at-point-functions
                     '(pcomplete-completions-at-point t))
    )

(use-package evil-org-agenda
    :hook (org-agenda-mode . evil-org-agenda-mode)
    :after org-agenda
    :config
    (evil-org-agenda-set-keys)
    ;; `evil-org-agenda-set=keys' binds SPC which occupies my local
    ;; leader key.  Bind SPC to nil before loading my own org-agenda
    ;; keymap.
    (general-define-key
     :states '(motion)
     :keymaps 'org-agenda-mode-map
     "SPC" nil))

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
          ;; NOTE: While I use `display-buffer-alist` to ensure the
          ;; Org Agenda can reuse an existing tab (one already
          ;; displaying the Org Agenda), this option remains
          ;; important. When I press "q," the tab closes. Without this
          ;; setting, pressing "q" causes the Org Agenda buffer to
          ;; exit, but the tab remains open, showing a different
          ;; buffer.
          org-agenda-window-setup 'other-tab)

    :config

    (add-to-list 'display-buffer-alist
                 '("^\\*Org Agenda"
                   (display-buffer-in-tab)
                   (tab-name . mg--get-tab-name)))

    ;; org-agenda will visit all org files listed
    ;; in `org-agenda-files' to generate the org-agenda view.
    ;; avoid too much files inside this directory.
    (setq  org-agenda-files `(,org-directory
                              ,@(mapcar
                                 (lambda (x) (file-name-concat org-directory x))
                                 '("capture" "work" "roam"))))

    (advice-add #'org-get-agenda-file-buffer :around #'mg-exclude-org-agenda-buffers-from-recentf)
    (add-hook 'org-agenda-finalize-hook #'mg-reload-org-agenda-buffers)

    (mg-localleader
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

        "q" #'org-agenda-set-tags
        "r" #'org-agenda-refile
        "t" #'org-agenda-todo

        )

    (general-define-key
     :keymaps 'org-agenda-mode-map
     :states '(normal visual motion)
     "gt" #'tab-bar-switch-to-next-tab
     "gT" #'tab-bar-switch-to-prev-tab)

    )

(use-package org-habit
    :init
    ;; show habits only on today in agenda buffer
    (setq org-habit-show-habits-only-for-today t)
    (add-to-list 'org-modules 'org-habit t))

(use-package ob
    :init
    (setq org-src-preserve-indentation t
          org-edit-src-content-indentation 0
          org-src-tab-acts-natively t
          ;; The confliction of `evil-want-C-i-jump' and `org-mode' is
          ;; fixed by the hack of `mg-org-indent-maybe-h'
          org-confirm-babel-evaluate nil
          org-link-elisp-confirm-function nil
          ;; Show src buffer in popup, and don't monopolize the frame
          org-src-window-setup 'plain)

    :config
    (add-to-list 'org-src-lang-modes '("r" . R))
    (add-to-list 'org-src-lang-modes '("python" . python-ts))
    (setf (alist-get "bash" org-src-lang-modes) 'bash-ts)
    (setf (alist-get "sh" org-src-lang-modes) 'bash-ts)

    (add-to-list 'display-buffer-alist
                 '("\\*Org Src"
                   (display-buffer-at-bottom)
                   (window-height . 0.8)))

    (let ((org-babel-langs '((latex . t)
                             (R . t)
                             (emacs-lisp . t)
                             (shell . t)
                             (python . t))))

        (when mg-jupyter-want-integration
            (push '(jupyter . t) org-babel-langs))

        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-langs)

        (when mg-jupyter-want-integration
            (setf (alist-get "jupyter-python" org-src-lang-modes) 'python-ts))
        )

    ;; HACK: when you want to use org-babel to execute a code block,
    ;; it seems that it uses the language identifer associate with
    ;; this block to query the `org-babel-execute:xxx' function.
    (defalias #'org-babel-execute:r #'org-babel-execute:R)
    (mg-org-babel-lsp-setup "R")
    (mg-org-babel-lsp-setup "python")
    (when mg-jupyter-want-integration
        ;; `org-babel-edit-prep:jupyter-python' (or other jupyter
        ;; kernels) will not be available at now, instead it will be
        ;; created as alias at the runtime after the kernel specs are
        ;; fetched. Since `org-babel-execute:jupyter-python' is just
        ;; an alias of `org-babel-execute:jupyter', we can just advice
        ;; `org-babel-execute:jupyter' here.
        (mg-org-babel-lsp-setup "jupyter"))

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
    (add-to-list 'org-export-backends 'md)
    (add-to-list 'org-export-backends 'beamer)
    (add-to-list 'org-export-backends 're-reveal)
    (delete 'odt org-export-backends))


(use-package evil-org
    :hook (org-mode . evil-org-mode)

    :config
    (evil-org-set-key-theme)
    (add-hook 'org-capture-mode-hook #'evil-insert-state))

;; make org fontification works similar to vim's conceal.
;; moving point over concealed text reveals its content.
(use-package org-appear
    :hook (org-mode . org-appear-mode)
    :init
    (setq org-appear-autolinks t
          org-appear-autoentities t)

    :config
    (when (display-graphic-p)
        (setq org-appear-autosubmarkers t)))

(use-package ob-jupyter
    :when mg-jupyter-want-integration
    :init
    (push 'zmq mg-load-incrementally-packages)
    :config
    (delq :text/html jupyter-org-mime-types))

(use-package org-re-reveal
    :init
    (setq org-re-reveal-root
          "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.9.0/"
          org-re-reveal-revealjs-version "3.9.0"))

(use-package org-download
    :commands (org-download-yank org-download-screenshot org-download-clipboard)
    :config
    (setq org-download-method 'attach
          org-download-screenshot-method
          (if IS-MAC
                  "screencapture -i %s"
              org-download-screenshot-method)))

(provide 'config-org)
;;; config-org.el ends here
