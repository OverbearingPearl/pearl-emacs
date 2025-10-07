(setq-default indent-tabs-mode nil)
(scroll-bar-mode -1)

;; Prevent automatic recentering when scrolling
;; Keep the cursor at the same screen position when possible
(setq scroll-conservatively 101)

;; Flash current line briefly on window changes
(defun my/flash-hl-line (&rest _)
  "Briefly highlight current line when window changes."
  (hl-line-mode 1)
  (run-with-timer 0.5 nil (lambda ()
                            (hl-line-mode -1))))

(add-hook 'window-selection-change-functions #'my/flash-hl-line)
(add-hook 'window-configuration-change-hook #'my/flash-hl-line)
(advice-add 'other-window :after #'my/flash-hl-line)

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  :config
  (setq rm-blacklist
        '(" company" " yas" " WK" " Undo-Tree")))

(provide 'my-ui)
