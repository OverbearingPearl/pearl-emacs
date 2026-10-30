(require 'my-preq)
(require 'auth-source)

(use-package gptel
  :config
  (require 'auth-source)

  (defconst my-gptel-vendors
    '(("DeepSeek" "api.deepseek.com" "/v1/responses"
       "low"
       (deepseek-flash))
      ("Zhipu" "open.bigmodel.cn" "/api/paas/v4/responses"
       nil
       (glm-5.3-flash glm-5.3-flashx))
      ("OfoxAI" "api.ofox.ai" "/v1/responses"
       "low"
       (z-ai/glm-5.3-flashx z-ai/glm-5.3-flash
        deepseek/deepseek-v4.1-flash))
      ("OpenRouter" "openrouter.ai" "/api/v1/responses"
       "low"
       (z-ai/glm-5.3-flashx z-ai/glm-5.3-flash
        xiaomi/mimo-v2.6-flash
        deepseek/deepseek-v4.1-flash
        openai/gpt-6-luna)))
    "Vendor specifications for gptel backends using the OpenAI Responses API.
The 4th element is a reasoning effort value (nil, the string
\"none\", or an effort string such as \"low\"/\"high\") from
which `my-gptel-request-params' derives the request parameters.")

  (defun my-gptel-request-params (effort)
    "Derive gptel request parameters from reasoning EFFORT.
EFFORT is nil, the string \"none\", or an effort string such as
\"low\"/\"high\".  A string effort explicitly enables reasoning
and configures only :top_p; \"none\" explicitly disables
reasoning and configures :temperature 0.0; nil configures
nothing at all---the :reasoning key and any sampling parameters
are omitted, deferring entirely to the endpoint defaults.  The
reasoning-on branch samples with :top_p only, and the
reasoning-off branch samples with :temperature only; the two
sampling parameter sets are never mixed in one request."
    (cond
      ;; Reasoning on: thinking models silently ignore temperature,
      ;; so top_p 0.95 (the floor of DeepSeek's thinking-mode
      ;; 0.95-1.0 range) is the only effective sampling knob.
      ((and effort (not (equal effort "none")))
       (list :reasoning (list :effort effort) :top_p 0.95))
      ;; Reasoning explicitly off ("none"): send the explicit
      ;; reasoning-off switch so the provider does not fall back
      ;; to its default reasoning behavior.
      ((equal effort "none")
       (list :reasoning (list :effort "none") :temperature 0.0))
      ;; Reasoning unconfigured (nil): send nothing at all; both
      ;; the :reasoning key and sampling parameters are omitted,
      ;; deferring entirely to the endpoint defaults.
      (t
       nil)))

  (defvar my-gptel-deepseek)
  (defvar my-gptel-zhipu)
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
             :request-params (my-gptel-request-params (nth 3 spec))
             :models (nth 4 spec)))))

  (defconst my-gptel-host-proxy-table
    '(("api.deepseek.com" . nil)
      ("open.bigmodel.cn" . nil)
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
