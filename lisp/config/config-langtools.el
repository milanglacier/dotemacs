;;; config-langtools.el -*- lexical-binding: t; -*-

(straight-use-package 'citre)
(straight-use-package '(eglot :type built-in))
(straight-use-package 'consult-eglot)
(straight-use-package 'edit-indirect)
(straight-use-package 'code-cells)
(straight-use-package 'reformatter)
(straight-use-package 'dape)

;; AI Code Completion
(straight-use-package '(minuet :host github :repo "milanglacier/minuet-ai.el"))

(use-package citre
    :init
    (require 'citre-config)
    (setq citre-tags-completion-case-sensitive nil)

    (add-hook 'emacs-lisp-mode-hook (mg-setq-locally citre-enable-imenu-integration nil))
    (add-hook 'emacs-lisp-mode-hook (mg-setq-locally citre-enable-capf-integration nil))
    (add-hook 'emacs-lisp-mode-hook (mg-setq-locally citre-enable-xref-integration nil))
    (add-hook 'org-mode-hook (mg-setq-locally citre-enable-imenu-integration nil))
    (add-hook 'org-mode-hook (mg-setq-locally citre-enable-capf-integration nil))
    (add-hook 'markdown-mode-hook (mg-setq-locally citre-enable-imenu-integration nil))

    :config

    (general-define-key
     :keymaps 'citre-mode-map
     :states '(normal motion)
     "C-]" #'citre-jump
     "C-t" #'citre-jump-back)

    (general-define-key
     :keymaps 'citre-mode-map
     :states '(normal motion visual)
     "C-w ]" #'citre-peek)

    (mg-find-map
        :keymaps 'citre-mode-map
        :states '(normal insert motion)
        "t" #'consult-citre) ;; find tags

    (general-define-key
     :prefix "SPC w"
     :non-normal-prefix "M-SPC w"
     :keymaps 'citre-mode-map
     :states '(normal visual insert motion)
     "]" #'citre-peek)

    (general-define-key
     :keymaps 'citre-peek-keymap
     :states '(normal motion)
     "q" #'citre-peek-abort
     "RET" #'citre-peek-jump
     "[t" #'citre-peek-prev-tag
     "]t" #'citre-peek-next-tag
     "M-k" #'citre-peek-prev-line
     "M-j" #'citre-peek-next-line
     "M-{" #'citre-peek-prev-branch
     "M-}" #'citre-peek-next-branch)

    (add-hook 'citre-mode-hook #'evil-normalize-keymaps)
    (add-hook 'citre-peek--mode-hook #'evil-normalize-keymaps)
    (add-hook 'citre-after-jump-hook #'better-jumper-set-jump)

    )

(use-package eldoc
    :init
    (setq eldoc-echo-area-use-multiline-p nil
          eldoc-documentation-strategy #'eldoc-documentation-compose)
    ;; eglot has 3 eldoc functions: `eglot-hover-eldoc-function', and
    ;; `eglot-signature-eldoc-function', using the default strategy
    ;; will only show one information, setting to the following option
    ;; allows the possibility to show both information in eldoc
    ;; buffer.

    :config
    (add-to-list 'display-buffer-alist
                 `("\\*eldoc\\*"
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (window-width . 0.5)
                   (window-height . 0.4)
                   (slot . ,(alist-get 'eldoc mg-side-window-slots))))
    ;; the major mode of eldoc buffer is special-mode.
    (general-define-key
     :states '(normal motion)
     :keymaps 'special-mode-map
     "q" #'quit-window)
    )

(use-package eglot
    :init
    (setq eglot-stay-out-of '(company)
          eglot-autoshutdown t
          eglot-workspace-configuration
          '(:basedpyright.analysis (:useLibraryCodeForTypes t :diagnosticMode "workspace" :autoSearchPaths t)
            ;; NOTE: I was thinking `python.analysis' should be a
            ;; hierarchical structure like { "python": { "analysis": {
            ;; xxx } }.  But examining the `eglot-event' buffer
            ;; suggests me that `python.analysis' should be a whole
            ;; component.
            :r (:lsp (:diagnostics t)))
          read-process-output-max (* 1024 1024)
          eglot-sync-connect 0)

    :config
    (add-to-list 'eglot-server-programs
                 '(python-ts-mode . ("basedpyright-langserver" "--stdio")))

    (add-to-list 'eglot-server-programs
                 '(sql-mode . ("sqls")))

    (add-to-list 'eglot-server-programs '(markdown-mode "efm-langserver"))

    (add-hook
     'eglot-managed-mode-hook #'mg-toggle-citre-eglot-capf)

    ;; NOTE: THIS IS REALLY IMPORTANT!
    ;; when you register evil keymaps for a minor mode keymap you MUST
    ;; call this func to automatically activate them otherwise, you
    ;; have to make a state transistion to make them become effective.
    (add-hook 'eglot-managed-mode-hook #'evil-normalize-keymaps)
    (add-hook 'eglot-managed-mode-hook
              (mg-setq-locally eldoc-documentation-function #'eldoc-documentation-compose))

    (general-create-definer mg-lsp-map
        :prefix "SPC l"
        :non-normal-prefix "M-SPC l"
        :prefix-map 'mg-lsp-map)

    (mg-lsp-map
     :keymaps 'eglot-mode-map
     :states '(normal insert motion visual)
     "" '(:ignore t :which-key "lsp")
     "f" #'mg-formatter
     "s" #'consult-eglot-symbols
     "a" #'eglot-code-actions
     "e" #'consult-flymake
     "p" #'flymake-show-project-diagnostics
     "n" #'eglot-rename
     "t" #'eglot-find-typeDefinition
     "i" #'eglot-find-implementation
     "[" #'xref-go-back
     "]" #'xref-go-forward)

    (general-define-key
     :keymaps 'eglot-mode-map
     :states '(normal motion)
     "] d" #'flymake-goto-next-error
     "[ d" #'flymake-goto-prev-error
     ;; jump to next/prev location containing the references.
     "[ r" (mg-xref-move-in-original-src-macro xref-prev-line)
     "] r" (mg-xref-move-in-original-src-macro xref-next-line)
     ;; jump to next/prev file containing the references.
     "[ R" (mg-xref-move-in-original-src-macro xref-prev-group)
     "] R" (mg-xref-move-in-original-src-macro xref-next-group)
     "K" #'mg-eldoc-buffer-dwim
     "gd" #'xref-find-definitions
     "gr" #'xref-find-references)

    )

(use-package edit-indiret
    :init
    (add-hook 'edit-indirect-after-creation-hook #'mg-markdown-src-lsp-setup)
    (add-to-list 'display-buffer-alist
                 '("\\*edit-indirect"
                   (display-buffer-at-bottom)
                   (window-height . 0.8))))

(use-package comint
    :config
    (general-define-key
     :states '(insert emacs)
     :keymaps 'comint-mode-map
     "C-a" #'comint-bol))


(use-package minuet
    ;; AI completion tool
    :init
    (general-define-key
     :states 'insert
     "M-y" #'minuet-complete-with-minibuffer
     "M-'" #'minuet-next-suggestion)

    :config
    (setq minuet-provider 'gemini)
    (setq minuet-request-timeout 2)
    (setq minuet-auto-suggestion-debounce-delay 0.5)
    (setq minuet-auto-suggestion-throttle-delay 1.5)

    (general-define-key
     :keymaps 'minuet-active-mode-map
     "M-;" #'minuet-previous-suggestion
     "M-'" #'minuet-next-suggestion
     "M-A" #'minuet-accept-suggestion
     "M-a" #'minuet-accept-suggestion-line
     "M-e" #'minuet-dismiss-suggestion)

    (minuet-set-optional-options minuet-gemini-options
                                 :generationConfig
                                 '(:maxOutputTokens 256
                                   :topP 0.9))
    (minuet-set-optional-options minuet-gemini-options
                                 :safetySettings
                                 [(:category "HARM_CATEGORY_DANGEROUS_CONTENT"
                                   :threshold "BLOCK_NONE")
                                  (:category "HARM_CATEGORY_HATE_SPEECH"
                                   :threshold "BLOCK_NONE")
                                  (:category "HARM_CATEGORY_HARASSMENT"
                                   :threshold "BLOCK_NONE")
                                  (:category "HARM_CATEGORY_SEXUALLY_EXPLICIT"
                                   :threshold "BLOCK_NONE")])

    (plist-put minuet-openai-compatible-options :end-point "https://openrouter.ai/api/v1/chat/completions")
    (plist-put minuet-openai-compatible-options :api-key "OPENROUTER_API_KEY")
    (plist-put minuet-openai-compatible-options :model "deepseek/deepseek-chat-v3-0324")
    ;; Prioritize throughput for faster completion
    (minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))

    (dolist (provider (list minuet-openai-options
                            minuet-codestral-options
                            minuet-openai-compatible-options
                            minuet-openai-fim-compatible-options))
        (minuet-set-optional-options provider :max_tokens 128)
        (minuet-set-optional-options provider :top_p 0.9))

    (minuet-set-optional-options minuet-codestral-options :stop ["\n\n"])
    )

(use-package treesit
    :init
    (setq treesit-language-source-alist
          '((bash "https://github.com/tree-sitter/tree-sitter-bash")
            (c "https://github.com/tree-sitter/tree-sitter-c")
            (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4")
            (css "https://github.com/tree-sitter/tree-sitter-css")
            (cmake "https://github.com/uyha/tree-sitter-cmake")
            (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp.git")
            (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
            (elisp "https://github.com/Wilfred/tree-sitter-elisp")
            (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
            (gomod "https://github.com/camdencheek/tree-sitter-go-mod.git")
            (html "https://github.com/tree-sitter/tree-sitter-html")
            (java "https://github.com/tree-sitter/tree-sitter-java.git")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
            (json "https://github.com/tree-sitter/tree-sitter-json")
            (lua "https://github.com/MunifTanjim/tree-sitter-lua")
            (make "https://github.com/alemuller/tree-sitter-make")
            (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src")
            (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src")
            (org "https://github.com/milisims/tree-sitter-org")
            (python "https://github.com/tree-sitter/tree-sitter-python")
            (php "https://github.com/tree-sitter/tree-sitter-php" nil "php/src")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
            (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")
            (sql "https://github.com/derekstride/tree-sitter-sql" "gh-pages")
            (vue "https://github.com/merico-dev/tree-sitter-vue")
            (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
            (toml "https://github.com/tree-sitter/tree-sitter-toml")
            (zig "https://github.com/GrayJack/tree-sitter-zig"))

          major-mode-remap-alist
          '((c-mode          . c-ts-mode)
            (c++-mode        . c++-ts-mode)
            (c-or-c++-mode   . c-or-c++-ts-mode)
            (cmake-mode      . cmake-ts-mode)
            (conf-toml-mode  . toml-ts-mode)
            (css-mode        . css-ts-mode)
            (js-mode         . js-ts-mode)
            (java-mode       . java-ts-mode)
            (js-json-mode    . json-ts-mode)
            (python-mode     . python-ts-mode)
            (sh-mode         . bash-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (rust-mode       . rust-ts-mode)
            (go-mode         . go-ts-mode))

          treesit-font-lock-level 4)

    (add-to-list 'auto-mode-alist '("CMakeLists\\'" . cmake-ts-mode))
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
    (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode))

    )

(use-package dape
    :commands (dape dape-breakpoint-toggle)

    :init
    (setq dape-repl-use-shorthand t
          dape-key-prefix nil)

    (add-hook 'python-ts-mode-hook #'mg--dape-keymap-setup)
    (add-hook 'go-ts-mode-hook #'mg--dape-keymap-setup)

    :config
    (mg-leader
        :keymaps 'override
        :states '(normal insert visual)
        "d" '(:keymap dape-global-map :which-key "DAP"))

    (general-define-key
     :keymaps 'dape-info-parent-mode-map
     :states 'normal
     "RET" #'dape-info-scope-watch-dwim
     "TAB" #'dape-info-scope-toggle
     "]]" #'mg-dape-info-goto-prev-tab
     "[[" #'mg-dape-info-goto-prev-tab)


    (general-define-key
     :keymaps 'dape-info-parent-mode-map
     ;; we want to unbind the default map of dape-info-parent-mode
     "<tab>" nil
     "<backtab>" nil)

    (setf (plist-get (alist-get 'debugpy dape-configs) 'command) "python3")

    )

(provide 'config-langtools)
;;; config-langtools.el ends here
