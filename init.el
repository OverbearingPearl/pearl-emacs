(dolist (dir '("common" "modules" "private"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))
