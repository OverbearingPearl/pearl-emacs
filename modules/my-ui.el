(setq-default indent-tabs-mode nil)
(scroll-bar-mode -1)

;; Prevent automatic recentering when scrolling
;; Keep the cursor at the same screen position when possible
(setq scroll-conservatively 101)

(setq comint-move-point-for-output t)
(setq comint-prompt-read-only t)
(defun my/scroll-to-bottom (_)
  (when (get-buffer-window (current-buffer))
    (with-selected-window (get-buffer-window (current-buffer))
      (goto-char (point-max))
      (recenter -1))))
(add-hook 'comint-output-filter-functions #'my/scroll-to-bottom)

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  :config
  (setq rm-blacklist
        '(" company" " yas" " WK" " Undo-Tree")))

(provide 'my-ui)
