(setq-default indent-tabs-mode nil)
(scroll-bar-mode -1)

;; Prevent automatic recentering when scrolling
;; Keep the cursor at the same screen position when possible
(setq scroll-conservatively 101)

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  :config
  (setq rm-blacklist
        '(" company" " yas" " WK" " Undo-Tree")))

(provide 'my-ui)
