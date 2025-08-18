(require 'prerequisite-check)

(use-package password-store
  :ensure t
  :if (and (prerequisite-check
            (executable "pass" :error-msg "password-store (pass) not found")
            (executable "gpg" :error-msg "GPG not found"))
           (let ((has-gpg-key (with-temp-buffer
                                (and (zerop (call-process "gpg" nil t nil "--list-secret-keys" "--keyid-format=long"))
                                     (> (buffer-size) 0)))))
             (unless has-gpg-key
               (warn "No GPG keys found. To generate a permanent key, run:
gpg --full-generate-key
Then choose:
1. Key type: (1) RSA and RSA
2. Key size: 4096
3. Expiration: 0 (key does not expire)
4. Real name, email and optional comment
5. Strong passphrase (recommended)"))
             (let ((pass-initialized (prerequisite-check (directory "~/.password-store"))))
               (unless pass-initialized
                 (warn "password-store not initialized. To initialize with your GPG key, run:
pass init <your-gpg-key-id>

To find your GPG key ID, run:
gpg --list-secret-keys --keyid-format=long
Look for the line starting with 'sec' and copy the key ID after the '/',
e.g. 9D479F6DAAD81B5E"))
               (and has-gpg-key pass-initialized)))))

(use-package aidermacs
  :ensure t
  :after password-store
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
  (let ((api-key (ignore-errors (password-store-get "code/deepseek_api_key"))))
    (unless api-key
      (let ((key (read-string "DeepSeek API key (required for first-time setup): ")))
        (when (ignore-errors (password-store-insert "code/deepseek_api_key" key))
          (setq api-key key)
          (message "API key stored successfully!"))))
    (when api-key
      (setenv "DEEPSEEK_API_KEY" api-key))))

(provide 'ai)
