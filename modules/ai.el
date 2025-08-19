(require 'prerequisite-check)

(use-package aidermacs
  :ensure t
  :if (prerequisite-check
       (executable "aider" :error-msg "aider CLI not found"))
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-default-model "deepseek/deepseek-coder")
  (setq aidermacs-extra-args
        '("--commit-language=en"
          "--commit-prompt=Write commit message following these guidelines:
1. First line: concise summary (max 50 chars)
2. (Optional) Additional details when necessary:
   - Simple changes may omit details
   - For complex changes consider:
     * Bullet points (- or •)
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
- Update documentation

Implementation notes:
Used bcrypt for password hashing..."))
  :init
  (let ((secret-file (expand-file-name "custom/secrets-plain.el" user-emacs-directory)))
    (when (file-exists-p secret-file)
      (load secret-file))
    (let ((api-key (and (boundp 'deepseek-api-key) deepseek-api-key)))
      (unless api-key
        (let ((key (read-string "DeepSeek API key (required for first-time setup): ")))
          (make-directory (file-name-directory secret-file) t)
          (with-temp-file secret-file
            (insert (format "(setq deepseek-api-key \"%s\")" key)))
          (setq api-key key)
          (message "API key stored in %s!" secret-file)))
      (when api-key
        (setenv "DEEPSEEK_API_KEY" api-key)))))

(provide 'ai)
