;;; minuet.el --- Code completion using LLM. -*- lexical-binding: t; -*-

;; Author: Milan Glacier <dev@milanglacier.com>
;; Maintainer: Milan Glacier <dev@milanglacier.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29") (plz "0.9") (dash "2.19.1"))

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
(defvar minuet-after-cursor-filter-length 15
    "Defines the length of non-whitespace context after the cursor
used to filter completion text. Set to 0 to disable filtering.

Example: With after_cursor_filter_length = 3 and context:

'def fib(n):\n|\n\nfib(5)' (where | represents cursor position),

if the completion text contains 'fib', then 'fib' and subsequent text
will be removed. This setting filters repeated text generated by the
LLM. A large value (e.g., 15) is recommended to avoid false
positives.")
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

- `<contextAfterCursor>`: Code context after the cursor
- `<cursorPosition>`: Current cursor location
- `<contextBeforeCursor>`: Code context before the cursor

Note that the user's code will be prompted in reverse order: first the code
after the cursor, then the code before the cursor.
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
<contextAfterCursor>

fib(5)
<contextBeforeCursor>
def fibonacci(n):
    <cursorPosition>")
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
      :end-point "https://codestral.mistral.ai/v1/fim/completions"
      :api-key "CODESTRAL_API_KEY"
      :optional nil)
    "config options for Minuet Codestral provider")

(defvar minuet-openai-compatible-options
    `(:end-point "https://api.groq.com/openai/v1/chat/completions"
      :api-key "GROQ_API_KEY"
      :model "llama-3.1-70b-versatile"
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
      :end-point "https://api.deepseek.com/beta/completions"
      :api-key "DEEPSEEK_API_KEY"
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

(defun minuet--remove-spaces (items)
    "Remove trailing and leading spaces in each item in items"
    ;; emacs use \\` and \\' to match the beginning/end of the string,
    ;; ^ and $ are used to match bol or eol
    (setq items (mapcar (lambda (x)
                            (if (or (equal x "")
                                    (string-match "\\`[\s\t\n]+\\'" x))
                                    nil
                                (setq x (replace-regexp-in-string "[\s\t\n]+\\'" "" x)
                                      x (replace-regexp-in-string "\\`[\s\t\n]+" "" x))))
                        items)
          items (seq-filter #'identity items))
    items)

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

(defun minuet--make-chat-llm-shot (context)
    (concat
     (plist-get context :additional)
     "\n<contextAfterCursor>\n"
     (plist-get context :after-cursor)
     "\n<contextBeforeCursor>\n"
     (plist-get context :before-cursor)
     "<cursorPosition>"))

(defun minuet--make-context-filter-sequence (context len)
    (if-let* ((is-string (stringp context))
              (is-positive (> len 0))
              (context (replace-regexp-in-string "\\`[\s\t\n]+" "" context))
              (should-filter (>= (length context) len))
              (context (substring context 0 len))
              (context (replace-regexp-in-string "[\s\t\n]+\\'" "" context)))
            context
        ""))

(defun minuet--filter-text (text sequence)
    "Remove the SEQUENCE and the rest part from TEXT."
    (cond
     ((or (null sequence) (null text)) text)
     ((equal sequence "") text)
     (t
      (let ((start (string-match-p (regexp-quote sequence) text)))
          (if start
                  (substring text 0 start)
              text)))))

(defun minuet--filter-sequence-in-items (items sequence)
    "For each item in ITEMS, apply `minuet--filter-text' with SEQUENCE."
    (mapcar (lambda (x) (minuet--filter-text x sequence))
            items))

(defun minuet--filter-context-sequence-in-items (items context)
    "Obtain the filter sequence from CONTEXT, and apply the filter sequence in each item in ITEMS."
    (minuet--filter-sequence-in-items
     items (minuet--make-context-filter-sequence
            (plist-get context :after-cursor)
            minuet-after-cursor-filter-length)))

(defun minuet--stream-decode (response get-text-fn)
    (setq response (split-string response "[\r]?\n"))
    (let (result)
        (dolist (line response)
            (if-let* ((json (condition-case err
                                    (json-parse-string
                                     (replace-regexp-in-string "^data: " "" line)
                                     :object-type 'plist :array-type 'list)
                                (error nil)))
                      (text (condition-case err
                                    (funcall get-text-fn json)
                                (error nil))))
                    (when (and (stringp text)
                               (not (equal text "")))
                        (setq result `(,@result ,text)))))
        (setq result (apply #'concat result))
        (if (equal result "")
                (progn (minuet--log (format "Minuet returns no text for streaming: %s" response))
                       nil)
            result)))

(defmacro minuet--make-process-stream-filter (response)
    "store the data into responses which is a plain list"
    `(lambda (proc text)
         (funcall #'internal-default-process-filter proc text)
         ;; (setq ,response (append ,response (list text)))
         (push text ,response)))

(defun minuet--stream-decode-raw (response get-text-fn)
    "Decode the raw stream stored in the temp variable create by `minuet--make-process-stream-filter'"
    (when-let* ((response (nreverse response))
                (response (apply #'concat response)))
        (minuet--stream-decode response get-text-fn)))

(defun minuet--handle-chat-completion-timeout (context err response get-text-fn name callback)
    "Handle the timeout error for chat completion.  This function will
decode and send the partial complete response to the callback,and log
the error"
    (if (equal (car (plz-error-curl-error err)) 28)
            (progn
                (minuet--log (format "%s Request timeout" name))
                (when-let* ((result (minuet--stream-decode-raw response get-text-fn))
                            (completion-items (minuet--parse-completion-itmes-default result))
                            (completion-items (minuet--filter-context-sequence-in-items
                                               completion-items
                                               context))
                            (completion-items (minuet--remove-spaces completion-items)))
                    (funcall callback completion-items)))
        (minuet--log (format "An error occured when sending request to %s" name))
        (minuet--log err)))

(defmacro minuet--with-temp-response (&rest body)
    "Execute BODY with a temporary response collection.
This macro creates a local variable `--response--' that can be used
to collect process output within the BODY. It's designed to work in
conjunction with `minuet--make-process-stream-filter'.
The `--response--' variable is initialized as an empty list and can
be used to accumulate text output from a process. After execution,
`--response--' will contain the collected responses in reverse order."
    `(let (--response--) ,@body))

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
                               items (-distinct items))
                         (completion-in-region (point) (point) items))))))

(defun minuet--check-env-var (env-var)
    (when-let ((var (getenv env-var)))
        (not (equal var ""))))

(defun minuet--codestral-available-p ()
    (minuet--check-env-var (plist-get minuet-codestral-options :api-key)))

(defun minuet--openai-available-p ()
    (minuet--check-env-var "OPENAI_API_KEY"))

(defun minuet--claude-available-p ()
    (minuet--check-env-var "ANTHROPIC_API_KEY"))

(defun minuet--openai-compatible-available-p ()
    (when-let* ((options minuet-openai-compatible-options)
                (env-var (plist-get options :api-key))
                (end-point (plist-get options :end-point))
                (model (plist-get options :model)))
        (minuet--check-env-var env-var)))

(defun minuet--openai-fim-compatible-available-p ()
    (when-let* ((options minuet-openai-fim-compatible-options)
                (env-var (plist-get options :api-key))
                (name (plist-get options :name))
                (end-point (plist-get options :end-point))
                (model (plist-get options :model)))
        (minuet--check-env-var env-var)))

(defun minuet--gemini-available-p ()
    (minuet--check-env-var "GEMINI_API_KEY"))

(defun minuet--parse-completion-itmes-default (items)
    (split-string items "<endCompletion>"))

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
            (minuet--with-temp-response
             (plz 'post (plist-get options :end-point)
                 :headers `(("Content-Type" . "application/json")
                            ("Accept" . "application/json")
                            ("Authorization" . ,(concat "Bearer " (getenv (plist-get options :api-key)))))
                 :timeout minuet-request-timeout
                 :body (json-serialize `(,@(plist-get options :optional)
                                         :stream t
                                         :model ,(plist-get options :model)
                                         :prompt ,(format "%s\n%s"
                                                          (plist-get context :additional)
                                                          (plist-get context :before-cursor))
                                         :suffix ,(plist-get context :after-cursor)))
                 :as 'string
                 :filter (minuet--make-process-stream-filter --response--)
                 :then
                 (lambda (json)
                     (setq try (1+ try))
                     (when-let ((result (minuet--stream-decode json get-text-fn)))
                         ;; insert the current result into the completion items list
                         (push result completion-items))
                     (when (>= try total-try)
                         (setq completion-items (minuet--filter-context-sequence-in-items
                                                 completion-items
                                                 context))
                         (setq completion-items (minuet--remove-spaces completion-items))
                         (funcall callback completion-items)))
                 :else
                 (lambda (err)
                     (setq try (1+ try))
                     (if (equal (car (plz-error-curl-error err)) 28)
                             (progn
                                 (minuet--log (format "%s Request timeout" name))
                                 (when-let ((result (minuet--stream-decode-raw --response-- get-text-fn)))
                                     (push result completion-items)))
                         (minuet--log (format "An error occured when sending request to %s" name))
                         (minuet--log err))
                     (when (>= try total-try)
                         (setq completion-items
                               (minuet--filter-context-sequence-in-items
                                completion-items
                                context))
                         (setq completion-items (minuet--remove-spaces completion-items))
                         (funcall callback completion-items))))))))

(defun minuet--codestral-complete (context callback)
    (minuet--openai-fim-complete-base
     (plist-put (copy-tree minuet-codestral-options) :name "Codestral")
     #'minuet--openai-get-text-fn
     context
     callback))

(defun minuet--openai-fim-compatible-complete (context callback)
    (minuet--openai-fim-complete-base
     (copy-tree minuet-openai-fim-compatible-options)
     #'minuet--openai-fim-get-text-fn
     context
     callback))

(defun minuet--openai-fim-get-text-fn (json)
    (--> json
         (plist-get it :choices)
         car
         (plist-get it :text)))

(defun minuet--openai-get-text-fn (json)
    (--> json
         (plist-get it :choices)
         car
         (plist-get it :delta)
         (plist-get it :content)))

(defun minuet--openai-complete-base (options context callback)
    (minuet--with-temp-response
     (plz 'post (plist-get options :end-point)
         :headers `(("Content-Type" . "application/json")
                    ("Accept" . "application/json")
                    ("Authorization" . ,(concat "Bearer " (getenv (plist-get options :api-key)))))
         :timeout minuet-request-timeout
         :body (json-serialize `(,@(plist-get options :optional)
                                 :stream t
                                 :model ,(plist-get options :model)
                                 :messages ,(vconcat
                                             `((:role "system"
                                                :content ,(minuet--make-system-prompt (plist-get options :system)))
                                               ,@(minuet--eval-value (plist-get options :few_shots))
                                               (:role "user"
                                                :content ,(minuet--make-chat-llm-shot context))))))
         :as 'string
         :filter (minuet--make-process-stream-filter --response--)
         :then
         (lambda (json)
             (when-let* ((result (minuet--stream-decode json #'minuet--openai-get-text-fn))
                         (completion-items (minuet--parse-completion-itmes-default result))
                         (completion-items (minuet--filter-context-sequence-in-items
                                            completion-items
                                            context))
                         (completion-items (minuet--remove-spaces completion-items)))
                 ;; insert the current result into the completion items list
                 (funcall callback completion-items)))
         :else
         (lambda (err)
             (minuet--handle-chat-completion-timeout
              context err --response-- #'minuet--openai-get-text-fn "OpenAI" callback)))))

(defun minuet--openai-complete (context callback)
    (minuet--openai-complete-base
     (--> (copy-tree minuet-openai-options)
          (plist-put it :end-point "https://api.openai.com/v1/chat/completions")
          (plist-put it :api-key "OPENAI_API_KEY"))
     context callback))

(defun minuet--openai-compatible-complete (context callback)
    (minuet--openai-complete-base
     (copy-tree minuet-openai-compatible-options) context callback))

(defun minuet--claude-get-text-fn (json)
    (--> json
         (plist-get it :delta)
         (plist-get it :text)))

(defun minuet--claude-complete (context callback)
    (minuet--with-temp-response
     (plz 'post "https://api.anthropic.com/v1/messages"
         :headers `(("Content-Type" . "application/json")
                    ("Accept" . "application/json")
                    ("x-api-key" . ,(getenv "ANTHROPIC_API_KEY"))
                    ("anthropic-version" . "2023-06-01"))
         :timeout minuet-request-timeout
         :body (json-serialize (let ((options (copy-tree minuet-claude-options)))
                                   `(,@(plist-get options :optional)
                                     :stream t
                                     :model ,(plist-get options :model)
                                     :system ,(minuet--make-system-prompt (plist-get options :system))
                                     :max_tokens ,(plist-get options :max_tokens)
                                     :messages ,(vconcat
                                                 `(,@(minuet--eval-value (plist-get options :few_shots))
                                                   (:role "user"
                                                    :content ,(minuet--make-chat-llm-shot context)))))))
         :as 'string
         :filter (minuet--make-process-stream-filter --response--)
         :then
         (lambda (json)
             (when-let* ((result (minuet--stream-decode json #'minuet--claude-get-text-fn))
                         (completion-items (minuet--parse-completion-itmes-default result))
                         (completion-items (minuet--filter-context-sequence-in-items
                                            completion-items
                                            context))
                         (completion-items (minuet--remove-spaces completion-items)))
                 ;; insert the current result into the completion items list
                 (funcall callback completion-items)))
         :else
         (lambda (err)
             (minuet--handle-chat-completion-timeout
              context err --response-- #'minuet--claude-get-text-fn "Claude" callback)))))

(defun minuet--gemini-get-text-fn (json)
    (--> json
         (plist-get it :candidates)
         car
         (plist-get it :content)
         (plist-get it :parts)
         car
         (plist-get it :text)))

(defun minuet--gemini-complete (context callback)
    (minuet--with-temp-response
     (plz 'post (format "https://generativelanguage.googleapis.com/v1beta/models/%s:streamGenerateContent?alt=sse&key=%s"
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
                                     :parts [(:text ,(minuet--make-chat-llm-shot context))]))))))
         :as 'string
         :filter (minuet--make-process-stream-filter --response--)
         :then
         (lambda (json)
             (when-let* ((result (minuet--stream-decode json #'minuet--gemini-get-text-fn))
                         (completion-items (minuet--parse-completion-itmes-default result))
                         (completion-items (minuet--filter-context-sequence-in-items
                                            completion-items
                                            context))
                         (completion-items (minuet--remove-spaces completion-items)))
                 (funcall callback completion-items)))
         :else
         (lambda (err)
             (minuet--handle-chat-completion-timeout
              context err --response-- #'minuet--gemini-get-text-fn "Gemini" callback)))))

(provide 'minuet)
;;; minuet.el ends here
