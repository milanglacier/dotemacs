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
(defvar minuet-context-ratio 0.75
    "when the total characters exceed the context window, the ratio of
context before cursor and after cursor, the larger the ratio the more
context before cursor will be used.")
(defvar minuet-request-timeout 3 "maximum timeout for sending request in second")
(defvar minuet-add-single-line-entry t
    "if completion item has multiple lines, create another completion
item only containing its first line.")
(defvar minuet-n-completions 3
    "The number of completion items (encoded as part of the prompt for
the chat LLM) requested from the language model. It's important to
note that when `minuet-add_single_line_entry' is set to true, the
actual number of returned items may exceed this value. Additionally,
the LLM cannot guarantee the exact number of completion items
specified, as this parameter serves only as a prompt guideline.")

(defvar minuet-default-prompt
    "You are the backend of an AI-powered code completion engine. Your task is to
provide code suggestions based on the user's input. The user's code will be
enclosed in markers:

- `<beginCode>`: Start of the code context
- `<cursorPosition>`: Current cursor location
- `<endCode>`: End of the code context
"
    "The default prompt for minuet completion")

(defvar minuet-default-guidelines
    "Guidelines:
1. Offer completions after the `<cursorPosition>` marker.
2. Make sure you have maintained the user's existing whitespace and indentation.
   This is REALLY IMPORTANT!
3. Provide multiple completion options when possible.
4. Return completions separated by the marker <endCompletion>.
5. The returned message will be further parsed and processed. DO NOT include
   additional comments or markdown code block fences. Return the result directly.
6. Keep each completion option concise, limiting it to a single line or a few lines."
    "The default guidelines for minuet completion")

(defvar minuet-default-n-completion-template
    "7. Provide at most %d completion items."
    "The default prompt for minuet for number of completions request.")

(defvar minuet-default-system-template
    "{{{:prompt}}}\n{{{:guidelines}}}\n{{{:n-completions-template}}}"
    "The default template for minuet system template")

(defvar minuet-default-fewshots
    `((:role "user"
       :content "# language: python
<beginCode>
def fibonacci(n):
    <cursorPosition>

fib(5)
<endCode>")
      (:role "assistant"
       :content "    '''
    Recursive Fibonacci implementation
    '''
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)
<endCompletion>
    '''
    Iterative Fibonacci implementation
    '''
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a + b
    return a
<endCompletion>
")))

(defvar minuet-claude-options
    `(:model "claude-3-5-sonnet-20240620"
      :max_tokens 512
      :system
      (:template minuet-default-system-template
       :prompt minuet-default-prompt
       :guidelines minuet-default-guidelines
       :n-completions-template minuet-default-n-completion-template)
      :few_shots minuet-default-fewshots
      :optional nil)
    "config options for Minuet Claude provider")

(defvar minuet-openai-options
    `(:model "gpt-4o-mini"
      :system
      (:template minuet-default-system-template
       :prompt minuet-default-prompt
       :guidelines minuet-default-guidelines
       :n-completions-template minuet-default-n-completion-template)
      :few_shots minuet-default-fewshots
      :optional nil)
    "config options for Minuet OpenAI provider")

(defvar minuet-codestral-options
    '(:model "codestral-latest"
      :end_point "https://codestral.mistral.ai/v1/fim/completions"
      :api_key "CODESTRAL_API_KEY"
      :optional nil)
    "config options for Minuet Codestral provider")

(defvar minuet-openai-compatible-options
    `(:end_point "https://api.mistral.ai/v1/chat/completions"
      :api_key "MISTRAL_API_KEY"
      :model "open-mistral-nemo"
      :system
      (:template minuet-default-system-template
       :prompt minuet-default-prompt
       :guidelines minuet-default-guidelines
       :n-completions-template minuet-default-n-completion-template)
      :few_shots minuet-default-fewshots
      :optional nil)
    "config options for Minuet OpenAI compatible provider")

(defvar minuet-openai-fim-compatible-options
    '(:model "deepseek-coder"
      :end_point "https://api.deepseek.com/beta/completions"
      :api_key "DEEPSEEK_API_KEY"
      :name "Deepseek"
      :optional nil)
    "config options for Minuet OpenAI FIM compatible provider")

(defvar minuet-gemini-options
    `(:model "gemini-1.5-flash-latest"
      :system
      (:template minuet-default-system-template
       :prompt minuet-default-prompt
       :guidelines minuet-default-guidelines
       :n-completions-template minuet-default-n-completion-template)
      :few_shots minuet-default-fewshots
      :optional nil)
    ;; (:generationConfig
    ;;  (:stopSequences nil
    ;;   :maxOutputTokens 256
    ;;   :topP 0.8))
    "config options for Minuet Gemini provider")

(defun minuet-set-optional-options (options key val &optional field)
    "Set the value of KEY in the FIELD of OPTIONS to VAL. If FIELD is
not provided, it defaults to :optional. If VAL is nil, then
remove KEY from OPTIONS. This helper function simplifies
setting values in a two-level nested plist structure."
    (let ((field (or field :optional)))
        (if val
                (setf (plist-get options field)
                      (plist-put (plist-get options field) key val))
            (setf (plist-get options field)
                  (map-delete (plist-get options field) key)))))

(defun minuet--eval-value (value)
    "If value is a function (either lambda or a callable symbol), eval
the function (with no argument) and return the result. Else if value
is a symbol, return its value. Else return itself."
    (cond ((functionp value) (funcall value))
          ((boundp value) (symbol-value value))
          (t value)))

(defun minuet--log (message &optional message-p)
    "Log minuet messages into `minuet-buffer-name'. Also print the message when MESSAGE-P is t."
    (with-current-buffer (get-buffer-create minuet-buffer-name)
        (goto-char (point-max))
        (insert (format "%s %s\n" message (format-time-string "%Y-%02m-%02d %02H:%02M:%02S")))
        (when message-p
            (message "%s" message))))

(defun minuet--add-tab-comment ()
    (if-let* ((language-p (derived-mode-p 'prog-mode 'text-mode 'conf-mode))
              (commentstring (format "%s %%s%s"
                                     (or comment-start "#")
                                     (or comment-end ""))))
            (if indent-tabs-mode
                    (format commentstring "indentation: use \t for a tab")
                (format commentstring (format "indentation: use %d spaces for a tab" tab-width)))
        ""))

(defun minuet--add-language-comment ()
    (if-let* ((language-p (derived-mode-p 'prog-mode 'text-mode 'conf-mode))
              (mode (symbol-name major-mode))
              (mode (replace-regexp-in-string "-ts-mode" "" mode))
              (mode (replace-regexp-in-string "-mode" "" mode))
              (commentstring (format "%s %%s%s"
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
            (cond ((< n-chars-before (* minuet-context-ratio minuet-context-window))
                   ;; If the context length before cursor does not exceed the maximum
                   ;; size, we include the full content before the cursor.
                   (setq context-after-cursor
                         (substring context-after-cursor 0 (- minuet-context-window n-chars-before))))
                  ((< n-chars-after (* (- 1 minuet-context-ratio) minuet-context-window))
                   ;; if the context length after cursor does not exceed the maximum
                   ;; size, we include the full content after the cursor.
                   (setq context-before-cursor
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
        `(:before-cursor ,context-before-cursor
          :after-cursor ,context-after-cursor
          :additional ,(format "%s\n%s" (minuet--add-language-comment) (minuet--add-tab-comment)))))

(defun minuet-encode-options (options key &optional override-key)
    "If the value of KEY from OPTIONS is not nil, then create a plist with KEY and its VALUE, otherwise return nil.
If OVERRIDE-KEY is provided, then use OVERRIDE-KEY as the key in the plist."
    (if-let ((value (plist-get options key)))
            `(,(or override-key key) ,value)))

;;;###autoload
(defun minuet-completion-in-region ()
    "Complete code in region with LLM."
    (interactive)
    (let ((current-buffer (current-buffer))
          (available-p-fn (intern (format "minuet--%s-available-p" minuet-provider)))
          (complete-fn (intern (format "minuet--%s-complete" minuet-provider)))
          (context (minuet--get-context))
          (point (point)))
        (unless (funcall available-p-fn)
            (minuet--log (format "Minuet provider %s is not available" minuet-provider))
            (error "Minuet provider %s is not available" minuet-provider))
        (funcall complete-fn
                 context
                 (lambda (items)
                     (with-current-buffer current-buffer
                         (setq items (if minuet-add-single-line-entry
                                             (minuet--add-single-line-entry items)
                                         items)
                               items (-distinct items)
                               items (mapcar
                                      (lambda (x)
                                          ;; . matches any chars except newline, so we have to also include newline in the group
                                          (replace-regexp-in-string "^\\([\s\n\t]*\\)\\(.\\|\n\\)*$" "" x nil nil 1))
                                      items))
                         (completion-in-region (point) (point) items))))))

(defun minuet--check-env-var (env-var)
    (when-let ((var (getenv env-var)))
        (not (equal var ""))))

(defun minuet--codestral-available-p ()
    (minuet--check-env-var (plist-get minuet-codestral-options :api_key)))

(defun minuet--openai-available-p ()
    (minuet--check-env-var "OPENAI_API_KEY"))

(defun minuet--claude-available-p ()
    (minuet--check-env-var "ANTHROPIC_API_KEY"))

(defun minuet--openai-compatible-available-p ()
    (when-let* ((options minuet-openai-compatible-options)
                (env-var (plist-get options :api_key))
                (end-point (plist-get options :end_point))
                (model (plist-get options :model)))
        (minuet--check-env-var env-var)))

(defun minuet--openai-fim-compatible-available-p ()
    (when-let* ((options minuet-openai-fim-compatible-options)
                (env-var (plist-get options :api_key))
                (name (plist-get options :name))
                (end-point (plist-get options :end_point))
                (model (plist-get options :model)))
        (minuet--check-env-var env-var)))

(defun minuet--gemini-available-p ()
    (minuet--check-env-var "GEMINI_API_KEY"))

(defun minuet--initial-process-completion-items-default (items)
    (setq
     items (split-string items "<endCompletion>")
     items (mapcar (lambda (x)
                       (if (or (equal x "")
                               (string-match "^[\s\t\n]+$" x))
                               nil
                           (setq x (replace-regexp-in-string "\n$" "" x)
                                 x (replace-regexp-in-string "^\n+" "" x))))
                   items)
     items (seq-filter #'identity items))
    items)

(defun minuet--make-system-prompt (template &optional n-completions)
    (let* ((tmpl (plist-get template :template))
           (tmpl (minuet--eval-value tmpl))
           (n-completions (or n-completions minuet-n-completions 1))
           (n-completions-template (plist-get template :n-completions-template))
           (n-completions-template (minuet--eval-value n-completions-template))
           (n-completions-template (if (stringp n-completions-template)
                                           (format n-completions-template (or minuet-n-completions 1))
                                       "")))
        (setq tmpl (replace-regexp-in-string "{{{:n-completions-template}}}"
                                             n-completions-template
                                             tmpl))
        (map-do
         (lambda (k v)
             (setq v (minuet--eval-value v))
             (when (and (not (equal k :template))
                        (not (equal k :n-completions-template))
                        (stringp v))
                 (setq tmpl
                       (replace-regexp-in-string
                        (concat "{{{" (symbol-name k) "}}}")
                        v
                        tmpl))))
         template)
        ;; replace placeholders that are not replaced
        (setq tmpl (replace-regexp-in-string "{{{.*}}}"
                                             ""
                                             tmpl))
        tmpl
        ))
(defun minuet--openai-fim-complete-base (options get-text-fn context callback)
    (let ((try 0)
          (total-try (or minuet-n-completions 1))
          (name (plist-get options :name))
          completion-items)
        (dotimes (_ total-try)
            (plz 'post (plist-get options :end_point)
                :headers `(("Content-Type" . "application/json")
                           ("Accept" . "application/json")
                           ("Authorization" . ,(concat "Bearer " (getenv (plist-get options :api_key)))))
                :timeout minuet-request-timeout
                :body (json-serialize `(,@(plist-get options :optional)
                                        :model ,(plist-get options :model)
                                        :prompt ,(format "%s\n%s"
                                                         (plist-get context :additional)
                                                         (plist-get context :before-cursor))
                                        :suffix ,(plist-get context :after-cursor)))
                :as 'string
                :then
                (lambda (json)
                    (setq try (1+ try))
                    (when-let* ((json (condition-case err
                                              (json-parse-string json :object-type 'plist :array-type 'list)
                                          (error (progn (minuet--log (format "Failed to parse %s API response" name))
                                                        nil))))
                                (choices (or (plist-get json :choices)
                                             (progn (minuet--log (format "%s API returns no content" name))
                                                    nil)))
                                (result (condition-case err
                                                (funcall get-text-fn choices)
                                            (error (progn (minuet--log (format "Failed to get text from %s" name))
                                                          nil)))))
                        ;; insert the current result into the completion items list
                        (push result completion-items)
                        (when (>= try total-try)
                            (funcall callback completion-items))))
                :else (lambda (err)
                          (setq try (1+ try))
                          (minuet--log (format "An error occured when sending request to %s" name))
                          (minuet--log err))))))

(defun minuet--codestral-complete (context callback)
    (minuet--openai-fim-complete-base
     (plist-put (copy-tree minuet-codestral-options) :name "Codestral")
     (lambda (choices)
         (--> choices car (plist-get it :message) (plist-get it :content)))
     context
     callback))

(defun minuet--openai-fim-compatible-complete (context callback)
    (minuet--openai-fim-complete-base
     (copy-tree minuet-openai-fim-compatible-options)
     (lambda (choices)
         (--> choices car (plist-get it :text)))
     context
     callback))

(defun minuet--openai-complete-base (options context callback)
    (plz 'post (plist-get options :end_point)
        :headers `(("Content-Type" . "application/json")
                   ("Accept" . "application/json")
                   ("Authorization" . ,(concat "Bearer " (getenv (plist-get options :api_key)))))
        :timeout minuet-request-timeout
        :body (json-serialize `(,@(plist-get options :optional)
                                :model ,(plist-get options :model)
                                :messages ,(vconcat
                                            `((:role "system"
                                               :content ,(minuet--make-system-prompt (plist-get options :system)))
                                              ,@(minuet--eval-value (plist-get options :few_shots))
                                              (:role "user"
                                               :content ,(concat
                                                          (plist-get context :additional)
                                                          "\n<beginCode>\n"
                                                          (plist-get context :before-cursor)
                                                          "<cursorPosition>"
                                                          (plist-get context :after-cursor)
                                                          "<endCode>"))))))
        :as 'string
        :then
        (lambda (json)
            (when-let* ((json (condition-case err
                                      (json-parse-string json :object-type 'plist :array-type 'list)
                                  (error (progn (minuet--log "Failed to parse OpenAI API response") nil))))
                        (choices (or (plist-get json :choices)
                                     (progn (minuet--log "OpenAI API returns no content") nil)))
                        (result (--> choices car (plist-get it :message) (plist-get it :content)))
                        (completion-items (minuet--initial-process-completion-items-default result)))
                ;; insert the current result into the completion items list
                (funcall callback completion-items)))
        :else (lambda (err)
                  (minuet--log "An error occured when sending request to OpenAI")
                  (minuet--log err))))

(defun minuet--openai-complete (context callback)
    (minuet--openai-complete-base
     (--> (copy-tree minuet-openai-options)
          (plist-put it :end_point "https://api.openai.com/v1/chat/completions")
          (plist-put it :api_key "OPENAI_API_KEY"))
     context callback))

(defun minuet--openai-compatible-complete (context callback)
    (minuet--openai-complete-base
     (copy-tree minuet-openai-compatible-options) context callback))

(defun minuet--claude-complete (context callback)
    (plz 'post "https://api.anthropic.com/v1/messages"
        :headers `(("Content-Type" . "application/json")
                   ("Accept" . "application/json")
                   ("x-api-key" . ,(getenv "ANTHROPIC_API_KEY"))
                   ("anthropic-version" . "2023-06-01"))
        :timeout minuet-request-timeout
        :body (json-serialize (let ((options (copy-tree minuet-claude-options)))
                                  `(,@(plist-get options :optional)
                                    :model ,(plist-get options :model)
                                    :system ,(minuet--make-system-prompt (plist-get options :system))
                                    :max_tokens ,(plist-get options :max_tokens)
                                    :messages ,(vconcat
                                                `(,@(minuet--eval-value (plist-get options :few_shots))
                                                  (:role "user"
                                                   :content ,(concat
                                                              (plist-get context :additional)
                                                              "\n<beginCode>\n"
                                                              (plist-get context :before-cursor)
                                                              "<cursorPosition>"
                                                              (plist-get context :after-cursor)
                                                              "<endCode>")))))))
        :as 'string
        :then
        (lambda (json)
            (when-let* ((json (condition-case err
                                      (json-parse-string json :object-type 'plist :array-type 'list)
                                  (error (progn (minuet--log "Failed to parse Claude API response") nil))))
                        (content (or (plist-get json :content)
                                     (progn (minuet--log "Claude API returns no content") nil)))
                        (result (--> content car (plist-get it :text)))
                        (completion-items (minuet--initial-process-completion-items-default result)))
                ;; insert the current result into the completion items list
                (funcall callback completion-items)))
        :else (lambda (err)
                  (minuet--log "An error occured when sending request to Claude")
                  (minuet--log err))))

(defun minuet--gemini-complete (context callback)
    (plz 'post (format "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s"
                       (plist-get minuet-gemini-options :model)
                       (getenv "GEMINI_API_KEY"))
        :headers `(("Content-Type" . "application/json")
                   ("Accept" . "application/json"))
        :timeout minuet-request-timeout
        :body (json-serialize
               (let* ((options (copy-tree minuet-gemini-options))
                      (few_shots (minuet--eval-value (plist-get options :few_shots)))
                      (few_shots (mapcar
                                  (lambda (shot)
                                      `(:role
                                        ,(if (equal (plist-get shot :role) "user") "user" "model")
                                        :parts
                                        [(:text ,(plist-get shot :content))]))
                                  few_shots)))
                   `(,@(plist-get options :optional)
                     :system_instruction (:parts (:text ,(minuet--make-system-prompt (plist-get options :system))))
                     :contents ,(vconcat
                                 `(,@few_shots
                                   (:role "user"
                                    :parts [(:text
                                             ,(concat
                                               (plist-get context :additional)
                                               "\n<beginCode>\n"
                                               (plist-get context :before-cursor)
                                               "<cursorPosition>"
                                               (plist-get context :after-cursor)
                                               "<endCode>"))]))))))
        :as 'string
        :then
        (lambda (json)
            (when-let* ((json (condition-case err
                                      (json-parse-string json :object-type 'plist :array-type 'list)
                                  (error (progn (minuet--log "Failed to parse Gemini API response") nil))))
                        (candidates (or (plist-get json :candidates)
                                        (progn (minuet--log "Gemini API returns no content") nil)))
                        (result (--> candidates
                                     car
                                     (plist-get it :content)
                                     (plist-get it :parts)
                                     car
                                     (plist-get it :text)))
                        (completion-items (minuet--initial-process-completion-items-default result)))
                ;; insert the current result into the completion items list
                (funcall callback completion-items)))
        :else (lambda (err)
                  (minuet--log "An error occured when sending request to Gemini")
                  (minuet--log err))))

(provide 'minuet)
;;; minuet.el ends here
