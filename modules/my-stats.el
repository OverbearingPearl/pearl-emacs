(require 'my-preq)
(require 'auth-source)

(use-package wakatime-mode
  :if (my-preq
       (executable "wakatime-cli" :error-msg "wakatime-cli not found") :on-fail silent)
  :config
  (let* ((source (car (auth-source-search :host "wakatime.com" :user "api-key")))
         (api-key (when source (funcall (plist-get source :secret)))))
    (unless api-key
      (setq api-key (read-string "Enter WAKATIME_API_KEY: ")))
    (when api-key
      (setenv "WAKATIME_API_KEY" api-key)
      (global-wakatime-mode 1))))

(provide 'my-stats)
