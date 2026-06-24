(require 'my-preq)

(use-package vterm
  :if (my-preq
       (executable "cmake" :error-msg "cmake not found")
       (executable "libtool" :error-msg "libtool not found")
       (executable "make" :error-msg "make not found")
       (executable "gcc" :error-msg "gcc not found"))
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-always-compile-module t))

(provide 'my-term)
