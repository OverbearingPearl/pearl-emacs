(require 'my-preq)
(require 'auth-source)

(use-package gptel
  :config
  (require 'auth-source)

  ;; Part 1: shared and opt-in request parameters.
  (defconst my-gptel-base-request-params
    '(:temperature 0.2)
    "Shared request parameters for all gptel backends.")

  (defconst my-gptel-reasoning-request-params
    '(:reasoning (:effort "low"))
    "Opt-in request parameters for low reasoning effort.")

  ;; Part 2: parameter helper.
  (defun my-gptel-request-params (&optional reasoning)
    "Return shared request parameters, optionally with low reasoning effort."
    (append (when reasoning my-gptel-reasoning-request-params)
            my-gptel-base-request-params))

  ;; Part 3: vendor specification as data.
  (defconst my-gptel-vendors
    '(("DeepSeek" "api.deepseek.com" "/v1/chat/completions" nil
       (deepseek-flash deepseek-v4-pro))
      ("Zhipu" "open.bigmodel.cn" "/api/paas/v4/chat/completions" nil
       (glm-5.3-flash))
      ("Qwen" "dashscope.aliyuncs.com" "/compatible-mode/v1/chat/completions" nil
       (qwen3.7-flash qwen3-coder-next))
      ("Xiaomi" "api.xiaomimimo.com" "/v1/chat/completions" nil
       (mimo-v2.6-flash))
      ("OfoxAI" "api.ofox.ai" "/v1/chat/completions" t
       (openai/gpt-5.6-luna
        anthropic/claude-4-opus
        z-ai/glm-5.3-flash z-ai/glm-5.3-flashx
        deepseek/deepseek-v4.1-flash))
      ("OpenRouter" "openrouter.ai" "/api/v1/chat/completions" t
       (deepseek/deepseek-v4-flash deepseek/deepseek-v4.1-flash
        qwen/qwen3.7-flash qwen/qwen3-coder-next
        z-ai/glm-5.3-flash
        xiaomi/mimo-v2.6-flash
        inclusionai/ling-3.0-flash-vl
        openai/gpt-5.6-luna openai/gpt-5.6-sol openai/gpt-6-luna openai/gpt-6-sol
        inception/mercury-2.5
        poolside/laguna-s-2.1)))
    "Vendor specifications for OpenAI-compatible gptel backends.")

  (defvar my-gptel-deepseek)
  (defvar my-gptel-zhipu)
  (defvar my-gptel-qwen)
  (defvar my-gptel-ofoxai)
  (defvar my-gptel-openrouter)

  (dolist (spec my-gptel-vendors)
    (let* ((name (nth 0 spec))
           (key (downcase name))
           (var (intern (format "my-gptel-%s" key))))
      (set var
           (gptel-make-openai name
             :host (nth 1 spec)
             :endpoint (nth 2 spec)
             :stream t
             :key #'my-gptel-api-key
             :curl-args #'my-gptel-curl-args
             :request-params (my-gptel-request-params (nth 3 spec))
             :models (nth 4 spec)))))

  ;; Part 4: proxy selection at request time.
  (defconst my-gptel-host-proxy-table
    '(("api.deepseek.com" . nil)
      ("open.bigmodel.cn" . nil)
      ("dashscope.aliyuncs.com" . nil)
      ("api.xiaomimimo.com" . nil)
      ("api.ofox.ai" . "127.0.0.1:7897")
      ("openrouter.ai" . "127.0.0.1:7897"))
    "Host-to-proxy mapping for gptel backends.")

  (defconst my-gptel-model-proxy-overrides nil
    "Per-model proxy overrides for gptel backends; a nil value forces a direct connection.")

  (defun my-gptel-curl-args ()
    "Return curl proxy arguments for the current gptel model."
    (let* ((model (format "%s" gptel-model))
           (host (and gptel-backend (gptel-backend-host gptel-backend)))
           (override (assoc model my-gptel-model-proxy-overrides))
           (proxy (if override
                      (cdr override)
                    (cdr (assoc host my-gptel-host-proxy-table)))))
      (when proxy
        (list "-x" proxy))))

  (defun my-gptel-api-key (&optional backend)
    "Return the API key for BACKEND, defaulting to the current backend."
    (let* ((backend (or backend gptel-backend))
           (host (and backend (gptel-backend-host backend))))
      (and host
           (auth-source-pick-first-password :host host)))))

(use-package scalpel
  :load-path "~/Projects/elisp/scalpel/"
  :after (gptel)
  :commands (scalpel-open)
  :custom
  (scalpel-prompt-reply-language "Chinese"))

(provide 'my-ai)
