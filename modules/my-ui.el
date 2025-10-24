(setq-default indent-tabs-mode nil)
(scroll-bar-mode -1)

;; Prevent automatic recentering when scrolling
;; Keep the cursor at the same screen position when possible
(setq scroll-conservatively 101)

;; Windmove configuration for easy window navigation
;; Only configure if windmove is available
(when (require 'windmove nil :noerror)
  (defun my/smart-window-switch ()
    "Smart window switching.
When only 2 windows exist, switch directly.
When more than 2 windows exist, use hjkl/HJKL keys for directional switching/swapping."
    (interactive)
    (let ((window-count (length (window-list))))
      (cond
       ((= window-count 1)
        (message "Only one window"))
       ((= window-count 2)
        (other-window 1))
       (t
        (message "Use h/j/k/l for move, H/J/K/L for swap")
        (let ((key (read-key "Window operation [h/j/k/l/H/J/K/L]: ")))
          (cl-case key
            (?h (windmove-left))
            (?j (windmove-down))
            (?k (windmove-up))
            (?l (windmove-right))
            (?H (windmove-swap-states-left))
            (?J (windmove-swap-states-down))
            (?K (windmove-swap-states-up))
            (?L (windmove-swap-states-right))
            (t (message "Invalid direction"))))))))

  ;; Smart window switching
  (global-set-key (kbd "C-x o") 'my/smart-window-switch))

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
