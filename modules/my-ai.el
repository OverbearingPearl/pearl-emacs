(require 'my-preq)
(require 'vc)

(use-package aidermacs
  :if (my-preq
       (executable "aider" :error-msg "aider CLI not found"))
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-default-model "deepseek/deepseek-coder")

  ;; Function to check for .aidermacs.prompting.md in git root, and copy template if needed
  (defun my/get-aidermacs-prompting-file ()
    (let ((git-root (vc-root-dir)))
      (when git-root
        (let ((prompt-file (expand-file-name ".aidermacs.prompting.md" git-root))
              (template-file (expand-file-name "custom/aidermacs.prompting.md.template" user-emacs-directory)))
          (cond
           ((file-exists-p prompt-file)
            prompt-file)
           ((file-exists-p template-file)
            (when (y-or-n-p (format "No .aidermacs.prompting.md found in project. Copy template from %s? " template-file))
              (copy-file template-file prompt-file)
              (message "Copied template to %s" prompt-file)
              prompt-file))
           (t nil))))))

  ;; Build the extra args list dynamically
  (defun my/build-aidermacs-extra-args ()
    (let ((base-args '("--commit-language=en"
                       "--commit-prompt=\"Write commit message following these guidelines:
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
- Update documentation\""))
          (prompt-file (my/get-aidermacs-prompting-file)))
      (if prompt-file
          (append base-args (list (concat "--read=" prompt-file)))
        base-args)))

  ;; Always update aidermacs-extra-args before using aidermacs commands
  (defun my/update-aidermacs-extra-args ()
    (setq aidermacs-extra-args (my/build-aidermacs-extra-args)))

  ;; Advise aidermacs commands to update the extra args before running
  (advice-add 'aidermacs-transient-menu :before #'my/update-aidermacs-extra-args)
  ;; Initialize aidermacs-extra-args
  (my/update-aidermacs-extra-args)
  :init
  (let ((api-key (and (boundp 'deepseek-api-key) deepseek-api-key))
        (secret-file (expand-file-name "custom/secrets.el" user-emacs-directory)))
    (unless api-key
      (let ((key (read-string "DeepSeek API key (required for first-time setup): ")))
        (with-temp-file secret-file
          (insert (format "(setq deepseek-api-key \"%s\")" key)))
        (setq api-key key)
        (message "API key stored in %s!" secret-file)))
    (when api-key
      (setenv "DEEPSEEK_API_KEY" api-key))))

(provide 'my-ai)
