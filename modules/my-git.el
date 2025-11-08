(require 'my-preq)

(use-package magit
  :if (my-preq
       (executable "git" :error-msg "git not found"))
  :config (magit-auto-revert-mode 1))

(use-package forge
  :if (my-preq
       (executable "git" :error-msg "git not found"))
  :config
  (setq forge-database-file (expand-file-name "forge-db.sqlite" user-emacs-directory)))

(provide 'my-git)
