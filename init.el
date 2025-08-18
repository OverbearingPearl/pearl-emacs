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
(when (or (not package-archive-contents)
          (time-less-p package-archive-contents-last-update
                       (time-subtract (current-time) (days-to-time 1))))
  (package-refresh-contents))

(package-initialize)

(require 'ai)
