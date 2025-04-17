;;; repl-macro-aider.el --- Aider integration for repl-macro -*- lexical-binding: t; -*-

;; Author: Milan Glacier <dev@milanglacier.com>
;; Maintainer: Milan Glacier <dev@milanglacier.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29") (repl-macro "0.1"))

;;; Commentary:

;; This package provides integration between repl-macro and Aider, a
;; LLM based code assistant.  It extends the functionality of
;; repl-macro to work seamlessly with Aider, allowing users to
;; interact with the AI assistant directly from within Emacs.

;; Features:
;; - Creates an Aider REPL session using eat
;; - Provides functions to start the Aider REPL and send commands
;; - Configures Aider-specific settings for optimal interaction

;; Usage:
;; M-x repm~aider-start

;;; Code:

(require 'repl-macro)

(defvar eat-buffer-name)
(defvar eat-shell)
(declare-function eat "eat")
(declare-function eat--send-string "eat")
(declare-function eat--synchronize-scroll "eat")

(defvar vterm-shell)
(defvar vterm-buffer-name)
(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")

(defvar repm-aider-prefixes
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

(defvar repm-aider-available-args
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

(defvar repm-aider-cmd "aider" "the command used to start the aider")
(defvar repm-aider-args "--watch-files" "the arguments used to start the aider")

(defun repm-aider-full-command ()
    "Return the full aider command with the args"
    (concat repm-aider-cmd
            (if (stringp repm-aider-args)
                    (concat " " repm-aider-args)
                "")))

;;;###autoload (autoload #'repm~aider-start "repl-macro-aider" nil t)
(repm-create-schema "aider"
                    #'repm-aider-full-command
                    :bracketed-paste-p t)

(defvaralias 'repm-aider-prefix 'repm*aider-start-pattern)

(defun repm-aider-set-prefix (prefix)
    "set the `repm-aider-prefix' which will be selected from `repm-aider-prefixes'"
    (interactive
     (list (completing-read "The prefixes passed to aider: "
                            repm-aider-prefixes
                            ;; require the prefix must be a member of
                            ;; `repm-aider-prefixes'
                            nil t)))
    (setq repm-aider-prefix (if (equal prefix "") "" (concat prefix " "))))


(defun repm-aider-remove-prefix ()
    "remove the prefix from `repm-aider-prefix'"
    (interactive)
    (setq repm-aider-prefix ""))

(defun repm-aider-set-args (args)
    "set the command line arguments to launcher aider"
    (interactive
     (list (completing-read "The arguments passed to aider: "
                            repm-aider-available-args
                            ;; do not require the argument must be a
                            ;; member of `repm-aider-available-args'
                            nil nil)))
    (setq repm-aider-args args))

(defun repm-aider-remove-args ()
    "remove the args from `repm*aider-cmd'"
    (interactive)
    (setq repm*aider-cmd repm-aider-cmd))

(defun repm-aider-prompt (prefix prompt &optional session)
    "Prompt aider with the given PREFIX and PROMPT verbatim.
Override `repm-aider-prefix' to ensure verbatim input."
    (interactive
     (list (completing-read "the aider command: " repm-aider-prefixes nil t)
           (read-string "enter your prompt: ")
           current-prefix-arg))
    (let* ((prefix (or prefix ""))
           (prompt (or prompt ""))
           (repm-aider-prefix
            (if (equal prefix "") "" (concat prefix " "))))
        (repm~aider-send-string prompt session)))

(defun repm-aider-yes (&optional session)
    "Send \"y\" to aider"
    (interactive "P")
    (repm-aider-prompt nil "y" session))

(defun repm-aider-no (&optional session)
    "Send \"n\" to aider"
    (interactive "P")
    (repm-aider-prompt nil "n" session))

(defun repm-aider-abort (&optional session)
    "Send C-c to aider"
    (interactive "P")
    (repm-aider-prompt nil "\C-c" session))

(defun repm-aider-exit (&optional session)
    (interactive "P")
    (repm-aider-prompt nil "\C-d" session))

(defun repm-aider-paste (&optional session)
    "Send \"/paste\" to aider"
    (interactive "P")
    (repm-aider-prompt nil "/paste" session))

(defun repm-aider-ask-mode (&optional session)
    "Send \"/ask\" to aider"
    (interactive "P")
    (repm-aider-prompt nil "/ask" session))

(defun repm-aider-arch-mode (&optional session)
    "Send \"/architect\" to aider"
    (interactive "P")
    (repm-aider-prompt nil "/architect" session))

(defun repm-aider-code-mode (&optional session)
    "Send \"/code\" to aider"
    (interactive "P")
    (repm-aider-prompt nil "/code" session))

(provide 'repl-macro-aider)
