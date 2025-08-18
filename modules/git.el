(require 'prerequisite-check)

(use-package magit
  :ensure t
  :if (prerequisite-check
       (executable "git" :error-msg "git nod found"))
  :config (magit-auto-revert-mode 1))

(provide 'git)
