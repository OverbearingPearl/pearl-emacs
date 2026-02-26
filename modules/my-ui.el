(setq-default indent-tabs-mode nil)
(scroll-bar-mode -1)

(defun my/set-english-font ()
  "Set English/Latin font to JetBrains Mono.
If the font is not found, display a warning with installation instructions.
This font is used alongside LXGW WenKai Mono for 2:1 monospace."
  (interactive)
  (if (find-font (font-spec :name "JetBrains Mono"))
      (progn
        ;; Set default font to JetBrains Mono with medium weight
        (set-face-attribute 'default nil
                           :family "JetBrains Mono"
                           :height 140   ; Fixed height for 2:1 monospace alignment
                           :weight 'medium)
        ;; Ensure Latin characters use JetBrains Mono
        (set-fontset-font t 'latin "JetBrains Mono")
        ;; Optional: adjust line spacing for better appearance
        (setq-default line-spacing 0.2))
    (message "Warning: JetBrains Mono font not found, using system default font.
Installation commands:
macOS (Homebrew): brew install font-jetbrains-mono
Ubuntu/Debian: sudo apt install fonts-jetbrains-mono
Download from: https://www.jetbrains.com/lp/mono/")))

(add-hook 'after-init-hook #'my/set-english-font)

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
       ((<= window-count 3)
        (other-window 1))
       ((> window-count 3)
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
