;;; -*- lexical-binding: t; -*-

(require 'my-preq)
(require 'auth-source)

(use-package gptel
  :config
  (require 'auth-source)

  (defconst my-gptel-vendors
    '(("DeepSeek" gptel-make-openai-responses "api.deepseek.com" "/v1/responses"
       (deepseek-flash))
      ("Zhipu" gptel-make-openai-responses "open.bigmodel.cn" "/api/v1/responses"
       (glm-5.3-flash glm-5.3-flashx))
      ("Qwen" gptel-make-openai "llm-l5plvn5p70xoh8n5.cn-beijing.maas.aliyuncs.com" "/compatible-mode/v1/chat/completions"
       (qwen3.8-max-0902 qwen3.8-max qwen3.7-plus qwen3.8-flash))
      ("Doubao" gptel-make-openai-responses "ark.cn-beijing.volces.com" "/api/v3/responses"
       (doubao-seed-2.1-pro doubao-seed-2.1-lite))
      ("Moonshot" gptel-make-openai-responses "api.moonshot.cn" "/v1/responses"
       (kimi-k3.1-pro kimi-k3 kimi-k2.6 kimi-k2.7-code))
      ("Xiaomi" gptel-make-openai-responses "api.mimo.mi.com" "/v1/responses"
       (mimo-v2.6-pro mimo-v2.6-flash))
      ("Tencent" gptel-make-openai-responses "tokenhub.tencentmaas.com" "/v1/responses"
       (hy4-preview hy3))
      ("MiniMax" gptel-make-openai-responses "api.minimax.cn" "/v1/responses"
       (MiniMax-M3.1-Flash MiniMax-M3))
      ("OfoxAI" gptel-make-openai-responses "api.ofox.ai" "/v1/responses"
       (deepseek/deepseek-v4.1-flash
        z-ai/glm-5.3-flash
        xiaomi/mimo-v2.6-flash
        minimax/minimax-m3
        openai/gpt-6-luna))
      ("OfoxAI-Chat" gptel-make-openai "api.ofox.ai" "/v1/chat/completions"
       (z-ai/glm-5.3-flashx))
      ("OpenRouter" gptel-make-openai-responses "openrouter.ai" "/api/v1/responses"
       (deepseek/deepseek-v4.1-flash
        z-ai/glm-5.3-flashx z-ai/glm-5.3-flash
        xiaomi/mimo-v2.6-flash
        openai/gpt-6-luna)))
    "Vendor specifications for gptel backends.
Each entry is a five-element list (NAME CONSTRUCTOR HOST ENDPOINT MODELS);
CONSTRUCTOR is the backend constructor and fixes the protocol used by
every model in the entry: `gptel-make-openai-responses' for the OpenAI
Responses API, `gptel-make-openai' for the chat completions API.  The
OfoxAI models are split across the OfoxAI and OfoxAI-Chat entries on
this basis.  Request parameters are configured separately in
`my/gptel-model-request-params'.")

  (defvar my-gptel-deepseek)
  (defvar my-gptel-zhipu)
  (defvar my-gptel-qwen)
  (defvar my-gptel-doubao)
  (defvar my-gptel-moonshot)
  (defvar my-gptel-xiaomi)
  (defvar my-gptel-tencent)
  (defvar my-gptel-minimax)
  (defvar my-gptel-ofoxai)
  (defvar my-gptel-ofoxai-chat)
  (defvar my-gptel-openrouter)

  (dolist (spec my-gptel-vendors)
    (let* ((name (nth 0 spec))
           (constructor (nth 1 spec))
           (key (downcase name))
           (var (intern (format "my-gptel-%s" key))))
      (set var
           (funcall constructor name
             :host (nth 2 spec)
             :endpoint (nth 3 spec)
             :stream t
             :key #'my-gptel-api-key
             :curl-args #'my-gptel-curl-args
             :models (nth 4 spec)))))

  (defconst my-gptel-host-proxy-table
    '(("api.deepseek.com" . nil)
      ("open.bigmodel.cn" . nil)
      ("llm-l5plvn5p70xoh8n5.cn-beijing.maas.aliyuncs.com" . nil)
      ("ark.cn-beijing.volces.com" . nil)
      ("api.moonshot.cn" . nil)
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
    (openai/gpt-6-luna               . (:reasoning (:effort "low") :sampling (:temperature 0.0 :presence_penalty 0 :frequency_penalty 0)))
    (qwen3.8-flash                   . (:reasoning (:effort "low") :sampling (:temperature 0.0 :presence_penalty 0 :frequency_penalty 0)))
    (doubao-seed-2.1-lite            . (:reasoning (:effort "low") :sampling (:temperature 0.0 :presence_penalty 0 :frequency_penalty 0)))
    (MiniMax-M3.1-Flash              . (:reasoning (:effort "low") :sampling (:temperature 0.0 :presence_penalty 0 :frequency_penalty 0))))
  "Per-model request parameters for gptel, one entry per model
registered in `my-gptel-vendors'.

Each entry is (MODEL . PARAMS) where PARAMS is split into two
categories: `:reasoning', the reasoning object handed to the
endpoint verbatim, uniformly `(:effort \"low\")' for every model
listed here; and `:sampling', a plist containing exactly
`:temperature 0.0', `:presence_penalty 0', and
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

(with-eval-after-load 'gptel
  (my-gptel-qwen-enable-session-cache))

(my/gptel--install-request-params)

(use-package scalpel
  :load-path "~/Projects/elisp/scalpel/"
  :after (gptel)
  :commands (scalpel-open)
  :custom
  (scalpel-prompt-reply-language "Chinese"))

(provide 'my-ai)
