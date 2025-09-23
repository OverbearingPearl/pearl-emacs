(require 'my-preq)

(use-package clojure-mode
  :if (my-preq
       (executable "java" :error-msg "Java not found")))

(use-package cider
  :if (my-preq
       (executable "java" :error-msg "Java not found"))
  :after clojure-mode
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

(use-package highlight-indent-guides
  :hook (clojure-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack)
  (setq highlight-indent-guides-character ?|)
  (setq highlight-indent-guides-auto-enabled nil)
  (defun my/set-indent-guide-colors ()
    (if (eq (frame-parameter nil 'background-mode) 'dark)
        ;; Dark theme colors
        (progn
          (set-face-foreground 'highlight-indent-guides-odd-face "#E0E0E0")
          (set-face-foreground 'highlight-indent-guides-even-face "#B0B0B0")
          (set-face-foreground 'highlight-indent-guides-character-face "#808080"))
      ;; Light theme colors
      (progn
        (set-face-foreground 'highlight-indent-guides-odd-face "#404040")
        (set-face-foreground 'highlight-indent-guides-even-face "#606060")
        (set-face-foreground 'highlight-indent-guides-character-face "#808080"))))
  (add-hook 'after-load-theme-hook #'my/set-indent-guide-colors)
  (my/set-indent-guide-colors))

(provide 'my-clojure)
