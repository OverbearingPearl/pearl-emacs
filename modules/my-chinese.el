(defun my/set-chinese-fonts ()
  "Set Chinese font to LXGW WenKai Mono.
If the font is not found, display a warning with installation instructions.
This font is used alongside JetBrains Mono for 2:1 monospace."
  (interactive)
  ;; Check if Chinese font exists
  (if (find-font (font-spec :name "LXGW WenKai Mono"))
      ;; Set Chinese font for CJK characters with fixed size
      ;; This ensures Chinese characters are exactly double width of English characters
      ;; Use 't' as the target fontset, which is more reliable than frame-parameter
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset
                          (font-spec :family "LXGW WenKai Mono" :size 16.0)))
    (message "Warning: LXGW WenKai Mono font not found, using system default Chinese font.
Installation commands:
macOS (Homebrew): brew install font-lxgw-wenkai
Ubuntu/Debian: sudo apt install fonts-lxgw-wenkai
Download from: https://github.com/lxgw/LxgwWenKai")))

;; Ensure Chinese font is set after English font
;; Since my-ui.el loads before my-chinese.el, my/set-english-font hook runs first
(add-hook 'after-init-hook #'my/set-chinese-fonts)

(define-key global-map (kbd "M-《")
            (lookup-key global-map (kbd "M-<")))
(define-key global-map (kbd "M-》")
            (lookup-key global-map (kbd "M->")))

(define-key global-map (kbd "M-「")
            (lookup-key global-map (kbd "M-{")))
(define-key global-map (kbd "M-」")
            (lookup-key global-map (kbd "M-}")))

(define-key global-map (kbd "M-：")
            (lookup-key global-map (kbd "M-:")))

(provide 'my-chinese)
