(require 'my-preq)
(require 'vc)
(require 'auth-source)

(use-package aidermacs
  :ensure nil
  :if (or
       (my-preq
        (executable "aider" :error-msg "aider CLI not found") :on-fail silent)
       (my-preq
        (executable "aider-ce" :error-msg "aider-ce not found") :on-fail silent))
  :load-path "~/Projects/aidermacs/"
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  ;; (setq aidermacs-show-diff-after-change nil)
  ;; (setq aidermacs-backend 'vterm)
  (setq aidermacs-model-filter-mode 'configured-first)

  (add-to-list 'aidermacs-project-read-only-files ".aidermacs.prompting.md")
  (add-to-list 'aidermacs-project-read-only-files "../.aidermacs.prompting.md")
  (add-to-list 'aidermacs-project-read-only-files (expand-file-name "~/.aidermacs.prompting.md"))

  ;; Build the extra args list dynamically
  (defun my/build-aidermacs-extra-args ()
    (let ((base-args '()))
      (setq base-args (cons "--model=openrouter/deepseek/deepseek-v3.2" base-args))
      (setq base-args (cons "--edit-format=whole" base-args))
      (setq base-args (cons "--editor-model=openrouter/qwen/qwen3-coder-next" base-args))
      (setq base-args (cons "--editor-edit-format=diff" base-args))
      (setq base-args (cons "--weak-model=openrouter/openai/gpt-oss-20b" base-args))
      (when (featurep 'my-chinese) (setq base-args (cons "--chat-language=zh" base-args)))
      (setq base-args (cons "--commit-language=en" base-args))
      (setq base-args (cons "--commit-prompt=\"Write commit message following these guidelines:
1. First line: concise summary (max 50 chars)
2. (Optional) Additional details when necessary:
   - Simple changes may omit details
   - For complex changes consider:
     * Bullet points (- • *)
     * Numbered lists (1. 2. 3.)
     * Multiple paragraphs
     * Code blocks where applicable

Examples:
Simple change:
Fix typo in README

Complex change:
Add user authentication

- Implement login/logout endpoints
- Add JWT token support
- Include rate limiting
  • 100 requests/minute
  • 500 requests/hour
- Update documentation\"" base-args))
      base-args))

  ;; Always update aidermacs-extra-args before using aidermacs commands
  (defun my/update-aidermacs-extra-args ()
    (setq aidermacs-extra-args (my/build-aidermacs-extra-args)))

  ;; Advise aidermacs commands to update the extra args before running
  (advice-add 'aidermacs-transient-menu :before #'my/update-aidermacs-extra-args)
  ;; Initialize aidermacs-extra-args
  (my/update-aidermacs-extra-args)
  :init
  (let* ((openrouter-source (car (auth-source-search :host "openrouter.ai" :user "api-key")))
         (openrouter-api-key (when openrouter-source (funcall (plist-get openrouter-source :secret)))))
    (unless openrouter-api-key
      (setq openrouter-api-key (read-string "Enter OPENROUTER_API_KEY: " nil nil "")))
    (when (and openrouter-api-key (not (string-empty-p openrouter-api-key)))
      (setenv "OPENROUTER_API_KEY" openrouter-api-key)
      (message "Set OPENROUTER_API_KEY")))
  (let* ((deepseek-source (car (auth-source-search :host "deepseek.com" :user "api-key")))
         (deepseek-api-key (when deepseek-source (funcall (plist-get deepseek-source :secret)))))
    (unless deepseek-api-key
      (setq deepseek-api-key (read-string "Enter DEEPSEEK_API_KEY (optional): " nil nil "")))
    (when (and deepseek-api-key (not (string-empty-p deepseek-api-key)))
      (setenv "DEEPSEEK_API_KEY" deepseek-api-key)
      (message "Set DEEPSEEK_API_KEY"))))

(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openrouter-key
    (lambda ()
      (getenv "OPENROUTER_API_KEY")))
   (chatgpt-shell-deepseek-key
    (lambda ()
      (getenv "DEEPSEEK_API_KEY")))
   (chatgpt-shell-model-version "openai/gpt-oss-20b"))
  :config
  (setq chatgpt-shell-swap-model-filter
        (lambda (models)
          (seq-filter (lambda (model)
                        (let ((key-fn (map-elt model :key)))
                          (when key-fn
                            (not (string-empty-p
                                  (or (funcall key-fn) ""))))))
                      models)))
  (setq url-proxy-services
        '(("http" . "127.0.0.1:7897")
          ("https" . "127.0.0.1:7897"))))

(provide 'my-ai)
