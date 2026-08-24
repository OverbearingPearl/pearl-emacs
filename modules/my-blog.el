(require 'my-preq)

(defgroup my-blog nil
  "Settings for the my-blog module."
  :group 'convenience)

(defcustom my-blog-hugo-base-dir "~/hugo-site"
  "Base directory of your Hugo site."
  :type 'directory
  :group 'my-blog)

(defcustom my-blog-hugo-section "post"
  "Default section (subdirectory under content/) where posts will be exported."
  :type 'string
  :group 'my-blog)

;; --- interactive configuration ---

(defconst my-blog--default-base-dir "~/hugo-site"
  "Default value for `my-blog-hugo-base-dir'.")

(defconst my-blog--default-section "post"
  "Default value for `my-blog-hugo-section'.")

(defun my-blog--already-customized-p ()
  "Return non-nil if the user has customized my-blog settings via `custom-file'."
  (or (get 'my-blog-hugo-base-dir 'saved-value)
      (get 'my-blog-hugo-section 'saved-value)))

(defun my-blog--configure (&optional force)
  "Interactively configure my-blog settings.
If FORCE is non-nil, always ask, even if already customized.
Save settings by calling `customize-save-variable'."
  (interactive "P")
  (when (or force (not (my-blog--already-customized-p)))
    (setq my-blog-hugo-base-dir
          (read-directory-name "Hugo site root directory: "
                               (expand-file-name my-blog-hugo-base-dir)))
    (setq my-blog-hugo-section
          (read-string "Default Hugo section: "
                       my-blog-hugo-section))
    (customize-save-variable 'my-blog-hugo-base-dir my-blog-hugo-base-dir)
    (customize-save-variable 'my-blog-hugo-section my-blog-hugo-section)
    (message "my-blog configured and saved.")))

(defun my-blog--maybe-configure ()
  "Ask user to configure my-blog if settings are still defaults.
Runs after initialization when the custom-file has been loaded."
  (unless noninteractive
    (my-blog--configure)))

(defun my-blog-configure ()
  "Interactively configure my-blog settings (always ask)."
  (interactive)
  (my-blog--configure t))

(add-hook 'after-init-hook #'my-blog--maybe-configure)

;; --- end interactive configuration ---

(use-package ox-hugo
  :ensure t
  :after org
  :if (my-preq
       (executable "hugo" :error-msg "Hugo static site generator not found"))
  :config
  (setq org-hugo-base-dir (expand-file-name my-blog-hugo-base-dir))
  (setq org-hugo-section my-blog-hugo-section)

  ;; Use <kbd> tags for keyboard keys in exports
  (setq org-hugo-use-code-for-kbd t)

  ;; Automatically update :lastmod front matter on export
  (setq org-hugo-auto-set-lastmod t))

(provide 'my-blog)
