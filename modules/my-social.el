(require 'my-preq)
(require 'auth-source)

(use-package elfeed
  :if (my-preq
       (executable "curl" :error-msg "curl not found for elfeed"))
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-extra-arguments '("-x" "http://127.0.0.1:7897"))
  (setq elfeed-feeds
        '("https://www.reddit.com/r/emacs/.rss"
          "https://www.reddit.com/r/gtd/.rss"
          "https://news.ycombinator.com/rss"
          "https://emacs-china.org/latest.rss"))
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (setq elfeed-update-interval (* 60 60)))

(use-package hackernews
  :if (my-preq
       (executable "curl" :error-msg "curl not found for hackernews"))
  :config
  (setq hackernews-cache-file (expand-file-name "hackernews-cache" user-emacs-directory)))

(provide 'my-social)
