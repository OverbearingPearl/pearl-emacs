(require 'my-preq)
(require 'vc)

(use-package aidermacs
  :if (my-preq
       (executable "aider" :error-msg "aider CLI not found"))
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-show-diff-after-change nil)

  ;; Add .aidermacs.prompting.md to project read-only files
  (add-to-list 'aidermacs-project-read-only-files ".aidermacs.prompting.md")

  ;; Build the extra args list dynamically
  (defun my/build-aidermacs-extra-args ()
    (let ((base-args '()))
      (setq base-args (cons "--model=openrouter/google/gemini-2.0-flash-001" base-args))
      (setq base-args (cons "--editor-model=openrouter/google/gemini-2.5-pro" base-args))
      (setq base-args (cons "--weak-model=openrouter/google/gemini-2.0-flash-001" base-args))
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
  (let ((api-key (and (boundp 'openrouter-api-key) openrouter-api-key)))
    (unless api-key
      (let ((key (read-string "OpenRouter API key (required for first-time setup): ")))
        (with-temp-file secret-file
          (insert (format "(setq openrouter-api-key \"%s\")" key)))
        (setq api-key key)
        (message "API key stored in secrets-plain.el!")))
    (when api-key
      (setenv "OPENROUTER_API_KEY" api-key))))

(provide 'my-ai)
