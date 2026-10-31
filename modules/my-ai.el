(require 'my-preq)
(require 'auth-source)

(use-package gptel
  :config
  (require 'auth-source)

  (defconst my-gptel-vendors
    '(("DeepSeek" "api.deepseek.com" "/v1/responses"
       (deepseek-flash))
      ("Zhipu" "open.bigmodel.cn" "/api/v1/responses"
       (glm-5.3-flash glm-5.3-flashx))
      ("Qwen" "dashscope.aliyuncs.com" "/compatible-mode/v1/responses"
       (qwen3.8-max qwen3.7-plus qwen3.8-flash))
      ("Doubao" "ark.cn-beijing.volces.com" "/api/v3/responses"
       (doubao-seed-2.1-pro doubao-seed-2.1-lite))
      ("Moonshot" "api.moonshot.ai" "/v1/responses"
       (kimi-k3.1-pro kimi-k3 kimi-k2.6 kimi-k2.7-code))
      ("Xiaomi" "api.mimo.mi.com" "/v1/responses"
       (mimo-v2.6-pro mimo-v2.6-flash))
      ("Tencent" "tokenhub.tencentmaas.com" "/v1/responses"
       (hy4-preview hy3))
      ("MiniMax" "api.minimax.cn" "/v1/responses"
       (MiniMax-M3.1-Flash MiniMax-M3))
      ("OfoxAI" "api.ofox.ai" "/v1/responses"
       (deepseek/deepseek-v4.1-flash
        z-ai/glm-5.3-flashx z-ai/glm-5.3-flash
        xiaomi/mimo-v2.6-flash
        openai/gpt-6-luna))
      ("OpenRouter" "openrouter.ai" "/api/v1/responses"
       (deepseek/deepseek-v4.1-flash
        z-ai/glm-5.3-flashx z-ai/glm-5.3-flash
        xiaomi/mimo-v2.6-flash
        openai/gpt-6-luna)))
    "Vendor specifications for gptel backends using the OpenAI Responses API.
Each vendor specification contains a name, host, endpoint, and model list.
Request parameters are configured separately in
`my/gptel-model-request-params`.")

  (defvar my-gptel-deepseek)
  (defvar my-gptel-zhipu)
  (defvar my-gptel-qwen)
  (defvar my-gptel-doubao)
  (defvar my-gptel-moonshot)
  (defvar my-gptel-xiaomi)
  (defvar my-gptel-tencent)
  (defvar my-gptel-minimax)
  (defvar my-gptel-ofoxai)
  (defvar my-gptel-openrouter)

  (dolist (spec my-gptel-vendors)
    (let* ((name (nth 0 spec))
           (key (downcase name))
           (var (intern (format "my-gptel-%s" key))))
      (set var
           (gptel-make-openai-responses name
             :host (nth 1 spec)
             :endpoint (nth 2 spec)
             :stream t
             :key #'my-gptel-api-key
             :curl-args #'my-gptel-curl-args
             :models (nth 3 spec)))))

  (defconst my-gptel-host-proxy-table
    '(("api.deepseek.com" . nil)
      ("open.bigmodel.cn" . nil)
      ("dashscope.aliyuncs.com" . nil)
      ("ark.cn-beijing.volces.com" . nil)
      ("api.moonshot.ai" . nil)
      ("api.mimo.mi.com" . nil)
      ("tokenhub.tencentmaas.com" . nil)
      ("api.minimax.cn" . nil)
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

(defconst my/gptel-model-request-params
  '((deepseek-flash                  . (:reasoning (:effort "low") :sampling (:temperature 0.0 :presence_penalty 0 :frequency_penalty 0)))
    (deepseek/deepseek-v4.1-flash    . (:reasoning (:effort "low") :sampling (:temperature 0.0 :presence_penalty 0 :frequency_penalty 0)))
    (glm-5.3-flash                   . (:reasoning (:effort "low") :sampling (:temperature 0.0 :presence_penalty 0 :frequency_penalty 0)))
    (glm-5.3-flashx                  . (:reasoning (:effort "low") :sampling (:temperature 0.0 :presence_penalty 0 :frequency_penalty 0)))
    (z-ai/glm-5.3-flashx             . (:reasoning (:effort "low") :sampling (:temperature 0.0 :presence_penalty 0 :frequency_penalty 0)))
    (z-ai/glm-5.3-flash              . (:reasoning (:effort "low") :sampling (:temperature 0.0 :presence_penalty 0 :frequency_penalty 0)))
    (xiaomi/mimo-v2.6-flash          . (:reasoning (:effort "low") :sampling (:temperature 0.0 :presence_penalty 0 :frequency_penalty 0)))
    (openai/gpt-6-luna               . (:reasoning (:effort "low") :sampling (:temperature 0.0 :presence_penalty 0 :frequency_penalty 0))))
  "Per-model request parameters for gptel, one entry per model
registered in `my-gptel-vendors'.

Each entry is (MODEL . PARAMS) where PARAMS is split into two
categories: `:reasoning', the reasoning object handed to the
endpoint verbatim, uniformly `(:effort \"low\")' for every model
listed here; and `:sampling', a plist containing exactly
`:temperature 0.7', `:presence_penalty 0', and
`:frequency_penalty 0'.  Sampling uses temperature, not `:top_p';
do not combine temperature and `:top_p'.  These sampling
parameters apply only to the models listed in this table.
`my/gptel--flatten-request-params' flattens the two categories
into a flat request parameter list, and
`my/gptel--install-request-params' applies the flattened result
as gptel model properties.  Any model absent from this table
receives no overrides at all: every request parameter falls back
entirely to the endpoint defaults.")

(defun my/gptel--flatten-request-params (entry)
  "Flatten one categorized ENTRY of `my/gptel-model-request-params'
into a request-params plist for gptel.

ENTRY is (MODEL . PARAMS) and PARAMS keys are categories.  A
boolean category value (nil or t) is a marker and contributes
nothing.  The :sampling category holds top-level parameter pairs
and is spliced flat.  It may contain multiple pairs, but
temperature and top_p must not both be set for a model.  Any other
category holding a plist or another atom is emitted as a single
(CATEGORY VALUE) pair, so nested API objects such as an OpenAI
reasoning object or an Anthropic thinking budget can be added to
the table later without changing this function."
  (let ((result nil)
        (rest (cdr entry)))
    (while rest
      (let ((category (pop rest))
            (value (pop rest)))
        (unless (booleanp value)
          (if (eq category :sampling)
              (setq result (append result (copy-sequence value)))
            (setq result (append result (list category value)))))))
    result))

(defun my/gptel--install-request-params ()
  "Flatten each entry in `my/gptel-model-request-params' and store the
result on the corresponding model symbol's :request-params property.

gptel reads the :request-params property at the end of its parameter
merge chain when building a request, so a value stored here overrides
both the backend and the global params for that model."
  (dolist (entry my/gptel-model-request-params)
    (put (car entry) :request-params
         (my/gptel--flatten-request-params entry))))

(my/gptel--install-request-params)

(use-package scalpel
  :load-path "~/Projects/elisp/scalpel/"
  :after (gptel)
  :commands (scalpel-open)
  :custom
  (scalpel-prompt-reply-language "Chinese"))

(provide 'my-ai)
