;;; minuet.el --- Code completion using LLM. -*- lexical-binding: t; -*-

;; Author: Milan Glacier <dev@milanglacier.com>
;; Maintainer: Milan Glacier <dev@milanglacier.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29") ("plz" "0.9"))

;;; Commentary:
;; This package implements an AI-powered code completion tool for Emacs. It
;; supports to use a variety of LLMs to generate code completions.

;;; Code:

(require 'plz)
(require 'dash)

(defgroup minuet nil
    "Minuet group."
    :group 'applications)

(defvar minuet-buffer-name "*minuet*" "The basename for minuet buffers")
(defvar minuet-provider 'codestral "The provider of minuet completion")
(defvar minuet-context-window 12800 "the maximum total characters of the context before and after cursor")
(defvar minuet-context-ratio 0.6
    "when the total characters exceed the context window, the ratio of
context before cursor and after cursor, the larger the ratio the more
context before cursor will be used.")
(defvar minuet-request-timeout 3 "maximum timeout for sending request in second")
(defvar minuet-add-single-line-entry t
    "if completion item has multiple lines, create another completion
item only containing its first line.")

(defvar minuet-codestral-options
    '(:max_tokens 128
      :stop "\n\n"
      :model "codestral-latest"
      :n_completions 1)
    "config options for Minuet when the provider is Codestral")

(defun minuet--log (message &optional message-p)
    "Log minuet messages into `minuet-buffer-name'. Also print the message when MESSAGE-P is t."
    (with-current-buffer (get-buffer-create minuet-buffer-name)
        (goto-char (point-max))
        (insert (format "%s %s\n" message (format-time-string "%Y-%02m-%02d %02H:%02M:%02S")))
        (when message-p
            (message "%s" message))))

(defun minuet--add-tab-comment ()
    (if (derived-mode-p 'prog-mode 'conf-mode)
            (let ((commentstring (format "%s%%s%s"
                                         (or comment-start "#")
                                         (or comment-end ""))))
                (if indent-tabs-mode
                        (format commentstring "indentation: use \t for a tab")
                    (format commentstring (format "indentation: use %d spaces for a tab" tab-width))))
        ""))

(defun minuet--add-language-comment ()
    (if-let* ((language-p (derived-mode-p 'prog-mode 'text-mode 'conf-mode))
              (mode (symbol-name major-mode))
              (mode (replace-regexp-in-string "-ts-mode" "" mode))
              (mode (replace-regexp-in-string "-mode" "" mode))
              (commentstring (format "%s%%s%s"
                                     (or comment-start "#")
                                     (or comment-end ""))))
            (format commentstring (concat "language: " mode))
        ""))

(defun minuet--add-single-line-entry (data)
    (cl-loop
     for item in data
     when (stringp item)
     append (list (car (split-string item "\n"))
                  item)))

(defun minuet--get-context ()
    (let* ((point (point))
           (n-chars-before point)
           (point-max (point-max))
           (n-chars-after (- point-max point))
           (context-before-cursor (buffer-substring-no-properties (point-min) point))
           (context-after-cursor (buffer-substring-no-properties point point-max)))
        ;; use some heuristic to decide the context length of before cursor and after cursor
        (when (>= (+ n-chars-before n-chars-after) minuet-context-window)
            (cond ((< n-chars-before (* 0.5 minuet-context-window))
                   ;; at the very beginning of the file
                   (setq context-after-cursor
                         (substring context-after-cursor 0 (- minuet-context-window n-chars-before))))
                  ((< n-chars-after (* 0.5 minuet-context-window))
                   ;; at the very end of the file
                   (setq context-before-curosr
                         (substring context-before-cursor (- (+ n-chars-before n-chars-after)
                                                             minuet-context-window))))
                  (t
                   ;; at the middle of the file, use the context_ratio to determine the allocation
                   (setq context-after-cursor
                         (substring context-after-cursor 0
                                    (floor
                                     (* minuet-context-window (- 1 minuet-context-ratio))))
                         context-before-cursor
                         (substring context-before-cursor
                                    (max 0 (- n-chars-before (floor (* minuet-context-window minuet-context-ratio)))))))))
        (cons (format "%s\n%s\n%s" (minuet--add-language-comment) (minuet--add-tab-comment) context-before-cursor)
              context-after-cursor)))

;;;###autoload
(defun minuet-completion-in-region ()
    "Complete code in region with LLM."
    (interactive)
    (let ((current-buffer (current-buffer))
          (complete-fn (intern (format "minuet--%s-complete" minuet-provider)))
          (context (minuet--get-context))
          (point (point)))
        (funcall complete-fn
                 (car context)
                 (cdr context)
                 (lambda (items)
                     (with-current-buffer current-buffer
                         (setq items (if minuet-add-single-line-entry
                                             (minuet--add-single-line-entry items)
                                         items)
                               items (-distinct items)
                               items (mapcar
                                      (lambda (x) (replace-regexp-in-string "^[\s\n\t]*" "" x))
                                      items))
                         (completion-in-region (point) (point) items))))))


(defun minuet--codestral-complete (context-before-cursor context-after-cursor callback)
    (let ((try 0)
          (total-try (plist-get minuet-codestral-options :n_completions))
          completion-items)
        (dotimes (_ total-try)
            (plz 'post "https://codestral.mistral.ai/v1/fim/completions"
                :headers `(("Content-Type" . "application/json")
                           ("Accept" . "application/json")
                           ("Authorization" . ,(concat "Bearer " (getenv "CODESTRAL_API_KEY"))))
                :timeout minuet-request-timeout
                :body (json-serialize `(:model ,(plist-get minuet-codestral-options :model)
                                        :prompt ,context-before-cursor
                                        :suffix ,context-after-cursor
                                        :max_tokens ,(plist-get minuet-codestral-options :max_tokens)
                                        :stop ,(plist-get minuet-codestral-options :stop)))
                :as 'string
                :then (lambda (json)
                          (setq try (1+ try))
                          (when-let* ((json (condition-case err
                                                    (json-parse-string json :object-type 'plist :array-type 'list)
                                                (error (progn (minuet--log "Failed to parse Codestral API response") nil))))
                                      (choices (or (plist-get json :choices)
                                                   (progn (minuet--log "No response from Codestral API") nil)))
                                      (result (--> choices car (plist-get it :message) (plist-get it :content))))
                              ;; insert the current result into the completion items list
                              (push result completion-items)
                              (when (>= try total-try)
                                  (funcall callback completion-items))))
                :else (lambda (err)
                          (setq try (1+ try))
                          (minuet--log "An error occured when sending request to Codestral"))))))

;;; minuet.el ends here
