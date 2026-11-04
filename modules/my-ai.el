;;; -*- lexical-binding: t; -*-

(require 'my-preq)
(require 'auth-source)

(use-package gptel
  :config
  (require 'auth-source)

  (defconst my-gptel-vendors
    '(("DeepSeek" gptel-make-openai "api.deepseek.com" "/v1/chat/completions" (deepseek-flash))
      ("Zhipu" gptel-make-openai "open.bigmodel.cn" "/api/paas/v4/chat/completions" (glm-5.3-flash glm-5.3-flashx))
      ("Qwen" gptel-make-openai "llm-l5plvn5p70xoh8n5.cn-beijing.maas.aliyuncs.com" "/compatible-mode/v1/chat/completions" (qwen3.8-max-0902 qwen3.8-max qwen3.7-plus qwen3.8-flash))
      ("Doubao" gptel-make-openai "ark.cn-beijing.volces.com" "/api/v3/chat/completions" (doubao-seed-2.1-pro doubao-seed-2.1-lite))
      ("Moonshot" gptel-make-openai "api.moonshot.cn" "/api/v1/chat/completions" (kimi-k3.1-pro kimi-k3 kimi-k2.6 kimi-k2.7-code))
      ("Xiaomi" gptel-make-openai "api.mimo.mi.com" "/v1/chat/completions" (mimo-v2.6-pro mimo-v2.6-flash))
      ("Tencent" gptel-make-openai "tokenhub.tencentmaas.com" "/v1/chat/completions" (hy4-preview hy3))
      ("MiniMax" gptel-make-openai "api.minimax.cn" "/v1/chat/completions" (MiniMax-M3.1-Flash MiniMax-M3))
      ("OfoxAI" gptel-make-openai "api.ofox.ai" "/v1/chat/completions" (deepseek/deepseek-v4.1-flash
                                                                        z-ai/glm-5.3-flash z-ai/glm-5.3-flashx
                                                                        xiaomi/mimo-v2.6-flash
                                                                        minimax/minimax-m3
                                                                        openai/gpt-6-luna))
      ("OpenRouter" gptel-make-openai "openrouter.ai" "/api/v1/chat/completions" (deepseek/deepseek-v4.1-flash
                                                                                  z-ai/glm-5.3-flashx z-ai/glm-5.3-flash
                                                                                  xiaomi/mimo-v2.6-flash
                                                                                  openai/gpt-6-luna)))
    "Vendor specifications for gptel backends.
Each entry is a five-element list (NAME CONSTRUCTOR HOST ENDPOINT MODELS);
CONSTRUCTOR is `gptel-make-openai' for every entry: all vendors are
registered on the OpenAI chat completions protocol, and the previous
Responses-protocol entries have been migrated to their chat completions
endpoints.  OfoxAI is a single entry covering all its models.  Request
")

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

(use-package scalpel
  :load-path "~/Projects/elisp/scalpel/"
  :after (gptel)
  :commands (scalpel-open)
  :custom
  (scalpel-prompt-reply-language "Chinese"))

(provide 'my-ai)
