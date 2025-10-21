(setq-default indent-tabs-mode nil)
(scroll-bar-mode -1)

;; Prevent automatic recentering when scrolling
;; Keep the cursor at the same screen position when possible
(setq scroll-conservatively 101)

;; Windmove configuration for easy window navigation
;; Only configure if windmove is available
(when (require 'windmove nil :noerror)
  ;; Set up basic windmove keybindings
  (windmove-default-keybindings 'shift)

  ;; Set keybindings for window swapping
  (global-set-key (kbd "M-S-<up>") 'windmove-swap-states-up)
  (global-set-key (kbd "M-S-<down>") 'windmove-swap-states-down)
  (global-set-key (kbd "M-S-<left>") 'windmove-swap-states-left)
  (global-set-key (kbd "M-S-<right>") 'windmove-swap-states-right))

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
