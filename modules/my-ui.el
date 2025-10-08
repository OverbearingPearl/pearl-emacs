(setq-default indent-tabs-mode nil)
(scroll-bar-mode -1)

;; Prevent automatic recentering when scrolling
;; Keep the cursor at the same screen position when possible
(setq scroll-conservatively 101)

(use-package beacon
  :config
  (beacon-mode 1)
  ;; Optional: customize beacon behavior
  (setq beacon-blink-when-window-scrolls nil
        beacon-blink-when-window-changes t
        beacon-blink-when-buffer-changes t))

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  :config
  (setq rm-blacklist
        '(" company" " yas" " WK" " Undo-Tree")))

(provide 'my-ui)
