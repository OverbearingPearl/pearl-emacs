(dolist (dir '("infra" "modules" "custom"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

(setq custom-file (expand-file-name "custom/custom-vars.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(require 'package)
(setq package-archives
      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(require 'prerequisite-check)

(use-package password-store
  :ensure t
  :if (prerequisite-check
       (executable "pass" :error-msg "password-store (pass) not found"))
  :config
  (unless (ignore-errors (password-store-get "code/deepseek_api_key"))
    (let ((key (read-string "DeepSeek API key (required for first-time setup): ")))
      (password-store-insert "code/deepseek_api_key" key))))

(use-package aidermacs
  :ensure t
  :after password-store
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
