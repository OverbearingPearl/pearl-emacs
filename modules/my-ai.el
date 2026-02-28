(require 'my-preq)
(require 'vc)
(require 'auth-source)

(use-package aidermacs
  :if (or
       (my-preq
        (executable "aider" :error-msg "aider CLI not found") :on-fail silent)
       (my-preq
        (executable "aider-ce" :error-msg "aider-ce not found") :on-fail silent))
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-show-diff-after-change nil)

  (add-to-list 'aidermacs-project-read-only-files "~/.aidermacs.prompting.md")
  (add-to-list 'aidermacs-project-read-only-files ".aidermacs.prompting.md")

  ;; Build the extra args list dynamically
  (defun my/build-aidermacs-extra-args ()
    (let ((base-args '()))
      ;; openrouter/deepseek/deepseek-v3.2     ($0.25/$0.40/M)
      ;; openrouter/deepseek/deepseek-r1       ($0.70/$2.50/M)
      ;; openrouter/minimax/minimax-m2.5       ($0.30/$1.10/M)
      ;; openrouter/minimax/minimax-m2.1       ($0.27/$0.95/M)
      ;; openrouter/moonshotai/kimi-k2.5       ($0.45/$2.20/M)
      ;; openrouter/qwen/qwen3-235b-a22b-2507  ($0.07/$0.10/M)
      ;; openrouter/qwen/qwen3-coder           ($0.22/$1.00/M)
      (setq base-args (cons "--model=openrouter/qwen/qwen3-235b-a22b-2507" base-args))
      (setq base-args (cons "--editor-model=openrouter/qwen/qwen3-235b-a22b-2507" base-args))
      (setq base-args (cons "--weak-model=openrouter/qwen/qwen3-235b-a22b-2507" base-args))
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
  (let* ((source (car (auth-source-search :host "openrouter.ai" :user "api-key")))
         (secret (plist-get source :secret))
         (api-key (funcall secret)))
    (when api-key
      (setenv "OPENROUTER_API_KEY" api-key)
      (message "Set OPENROUTER_API_KEY: %s" api-key))))

(provide 'my-ai)
