;;; -*- lexical-binding: t; -*-

(require 'my-preq)

(use-package magit
  :if (my-preq
       (executable "git" :error-msg "git not found"))
  :config (magit-auto-revert-mode 1))

(use-package forge
  :after magit
  :config
  ;; Don't reflow issue/pull-request bodies when rendering them; forge's
  ;; `fill-region' hard-wraps long lines at `fill-column' and makes the
  ;; repository's templates look like they were modified.
  (setq forge-post-fill-region nil)
  (setq forge-database-file (expand-file-name "forge-db.sqlite" user-emacs-directory)))

(provide 'my-git)
