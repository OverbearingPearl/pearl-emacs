(dolist (dir '("infra" "modules" "custom"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

(setq custom-file (expand-file-name "custom/custom-vars.el" user-emacs-directory))
(make-directory (file-name-directory custom-file) t)
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(defcustom secret-file (expand-file-name "custom/secrets-plain.el" user-emacs-directory)
  "Path to the file containing secret configurations like API keys."
  :type 'file
  :group 'custom)
(make-directory (file-name-directory secret-file) t)
(unless (file-exists-p secret-file)
  (write-region "" nil secret-file))
(load secret-file)

(defcustom feature-file (expand-file-name "custom/features.el" user-emacs-directory)
  "Path to the file containing feature configurations."
  :type 'file
  :group 'custom)
(make-directory (file-name-directory feature-file) t)
(unless (file-exists-p feature-file)
  (let ((template-file (expand-file-name "custom/features.el.template" user-emacs-directory)))
    (copy-file template-file feature-file t)))

(require 'package)
(setq package-archives
      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(if (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

(load feature-file)
