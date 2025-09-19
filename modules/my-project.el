(use-package projectile
  :hook (after-init . projectile-mode)
  :bind ("C-c p" . 'projectile-commander)
  :config
  (setq projectile-use-git-grep t))

(provide 'my-project)
