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

(provide 'my-clojure)
