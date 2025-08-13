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
    (warn "API key not found in password store")))

(use-package aidermacs
  :ensure t
  :after password-store
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-default-model "deepseek/deepseek-coder")
  :hook
  (aidermacs-before-run-backend
   .
   (lambda ()
     (unless (getenv "DEEPSEEK_API_KEY")
       (let ((key (read-string "DeepSeek API key: "
                               nil nil
                               (password-store-get "code/deepseek_api_key"))))
	 (setenv "DEEPSEEK_API_KEY" key)
         (password-store-insert "code/deepseek_api_key" key))))))
