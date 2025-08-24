(require 'my-preq)

(use-package magit
  :if (my-preq
       (executable "git" :error-msg "git not found"))
  :config (magit-auto-revert-mode 1))

(provide 'git)
