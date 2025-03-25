;;; vterm-repl-aider.el --- Aider integration for vterm-repl -*- lexical-binding: t; -*-

;; Author: Milan Glacier <dev@milanglacier.com>
;; Maintainer: Milan Glacier <dev@milanglacier.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29") (vterm-repl "0.1"))

;;; Commentary:

;; This package provides integration between vterm-repl and Aider, a
;; LLM based code assistant.  It extends the functionality of
;; vterm-repl to work seamlessly with Aider, allowing users to
;; interact with the AI assistant directly from within Emacs.

;; Features:
;; - Creates an Aider REPL session using vterm
;; - Provides functions to start the Aider REPL and send commands
;; - Configures Aider-specific settings for optimal interaction

;; Usage:
;; M-x vtr~aider-start

;;; Code:

(require 'vterm-repl)

(defvar vtr-aider-prefixes
    '(""
      "/add"
      "/architect"
      "/ask"
      "/chat-mode"
      "/clear"
      "/code"
      "/commit"
      "/copy"
      "/copy-context"
      "/diff"
      "/drop"
      "/editor"
      "/exit"
      "/git"
      "/help"
      "/lint"
      "/load"
      "/ls"
      "/map"
      "/map-refresh"
      "/model"
      "/models"
      "/multiline-mode"
      "/paste"
      "/quit"
      "/read-only"
      "/report"
      "/reset"
      "/run"
      "/save"
      "/settings"
      "/test"
      "/tokens"
      "/undo"
      "/voice"
      "/web"
      "/think-tokens"
      "/reasoning-effort"
      )
    "the available command prefixes used by aider")

(defvar vtr-aider-available-args
    '("--reasoning-effort"
      "--watch-files"
      "--model"
      "--opus"
      "--sonnet"
      "--4"
      "--4o"
      "--mini"
      "--4-turbo"
      "--35turbo"
      "--deepseek"
      "--o1-mini"
      "--o1-preview"
      "--architect"
      "--weak-model"
      "--editor-model"
      "--editor-edit-format"
      "--show-model-warnings"
      "--no-show-model-warnings"
      "--cache-prompts"
      "--no-cache-prompts"
      "--map-refresh"
      "--map-multiplier-no-files"
      "--restore-chat-history"
      "--no-restore-chat-history"
      "--pretty"
      "--no-pretty"
      "--stream"
      "--no-stream"
      "--user-input-color"
      "--tool-output-color"
      "--tool-error-color"
      "--tool-warning-color"
      "--assistant-output-color"
      "--completion-menu-color"
      "--completion-menu-bg-color"
      "--completion-menu-current-color"
      "--completion-menu-current-bg-color"
      "--code-theme"
      "--show-diffs"
      "--git"
      "--no-git"
      "--gitignore"
      "--no-gitignore"
      "--aiderignore"
      "--subtree-only"
      "--auto-commits"
      "--no-auto-commits"
      "--dirty-commits"
      "--no-dirty-commits"
      "--attribute-author"
      "--no-attribute-author"
      "--attribute-committer"
      "--no-attribute-committer"
      "--attribute-commit-message-author"
      "--no-attribute-commit-message-author"
      "--attribute-commit-message-committer"
      "--no-attribute-commit-message-committer"
      "--commit"
      "--commit-prompt"
      "--dry-run"
      "--no-dry-run"
      "--skip-sanity-check-repo"
      "--lint"
      "--lint-cmd"
      "--auto-lint"
      "--no-auto-lint"
      "--test-cmd"
      "--auto-test"
      "--no-auto-test"
      "--test"
      "--file"
      "--read"
      "--vim"
      "--chat-language"
      "--install-main-branch"
      "--apply"
      "--yes-always"
      "-v"
      "--show-repo-map"
      "--show-prompts"
      "--message"
      "--message-file"
      "--encoding"
      "-c"
      "--gui"
      "--suggest-shell-commands"
      "--no-suggest-shell-commands"
      "--voice-format"
      "--voice-language"
      "--multiline"
      "--reasoning-effort"
      "--thinking-tokens"
      "--auto-accept-architect"
      "--no-auto-accept-architect"
      )
    "the available command arguments used by aider")

(defvar vtr-aider-cmd "aider" "the command used to start the aider")
(defvar vtr-aider-args "--watch-files" "the arguments used to start the aider")

(defun vtr-aider-full-command ()
    "Return the full aider command with the args"
    (concat vtr-aider-cmd
            (if (stringp vtr-aider-args)
                    (concat " " vtr-aider-args)
                "")))

;;;###autoload (autoload #'vtr~aider-start "vterm-repl-aider" nil t)
(vtr-create-schema "aider"
                   #'vtr-aider-full-command
                   :bracketed-paste-p t
                   :start-pattern "/ask ")

(defvaralias 'vtr-aider-prefix 'vtr*aider-start-pattern)

(defun vtr-aider-set-prefix (prefix)
    "set the `vtr-aider-prefix' which will be selected from `vtr-aider-prefixes'"
    (interactive
     (list (completing-read "The prefixes passed to aider: "
                            vtr-aider-prefixes
                            ;; require the prefix must be a member of
                            ;; `vtr-aider-prefixes'
                            nil t)))
    (setq vtr-aider-prefix (if (equal prefix "") "" (concat prefix " "))))


(defun vtr-aider-remove-prefix ()
    "remove the prefix from `vtr-aider-prefix'"
    (interactive)
    (setq vtr-aider-prefix ""))

(defun vtr-aider-set-args (args)
    "set the command line arguments to launcher aider"
    (interactive
     (list (completing-read "The arguments passed to aider: "
                            vtr-aider-available-args
                            ;; do not require the argument must be a
                            ;; member of `vtr-aider-available-args'
                            nil nil)))
    (setq vtr-aider-args args))

(defun vtr-aider-remove-args ()
    "remove the args from `vtr*aider-cmd'"
    (interactive)
    (setq vtr*aider-cmd vtr-aider-cmd))

(defun vtr-aider-prompt (prefix prompt &optional session)
    "Prompt aider with the given PREFIX and PROMPT verbatim.
Override `vtr-aider-prefix' to ensure verbatim input."
    (interactive
     (list (completing-read "the aider command: " vtr-aider-prefixes nil t)
           (read-string "enter your prompt: ")
           current-prefix-arg))
    (let* ((prefix (or prefix ""))
           (prompt (or prompt ""))
           (vtr-aider-prefix
            (if (equal prefix "") "" (concat prefix " "))))
        (vtr~aider-send-string prompt session)))

(defun vtr-aider-yes (&optional session)
    "Send \"y\" to aider"
    (interactive "P")
    (vtr-aider-prompt nil "y" session))

(defun vtr-aider-no (&optional session)
    "Send \"n\" to aider"
    (interactive "P")
    (vtr-aider-prompt nil "n" session))

(defun vtr-aider-abort (&optional session)
    "Send C-c to aider"
    (interactive "P")
    (vtr-aider-prompt nil "\C-c" session))

(defun vtr-aider-exit (&optional session)
    (interactive "P")
    (vtr-aider-prompt nil "\C-d" session))

(defun vtr-aider-paste (&optional session)
    "Send \"/paste\" to aider"
    (interactive "P")
    (vtr-aider-prompt nil "/paste" session))

(defun vtr-aider-ask-mode (&optional session)
    "Send \"/ask\" to aider"
    (interactive "P")
    (vtr-aider-prompt nil "/ask" session))

(defun vtr-aider-arch-mode (&optional session)
    "Send \"/architect\" to aider"
    (interactive "P")
    (vtr-aider-prompt nil "/architect" session))

(defun vtr-aider-code-mode (&optional session)
    "Send \"/code\" to aider"
    (interactive "P")
    (vtr-aider-prompt nil "/code" session))

(provide 'vterm-repl-aider)
