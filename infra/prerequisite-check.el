;; prerequisite-check.el --- Utility functions for checking prerequisites

;;; Commentary:

;; This module provides a convenient macro for checking various prerequisites
;; in your Emacs configuration or packages. It's particularly useful in init.el
;; to ensure required tools and environment variables are available before
;; loading specific configurations.
;;
;; Public Interface:
;; ----------------
;; The only public interface is the `prerequisite-check' macro, which accepts
;; multiple prerequisites and optional behavior modifiers.
;;
;; Basic Usage:
;;   (prerequisite-check
;;    '(executable "git" "2.0.0")
;;    '(env-var "HOME")
;;    '(file "/tmp/log.txt")
;;    '(directory "/tmp/build"))
;;
;; Available Prerequisites:
;; 1. Executable Check:
;;    Verifies program existence and optional version
;;    Format: '(executable PROGRAM-NAME [MIN-VERSION])
;;    Example: '(executable "git" "2.0.0")
;;
;; 2. Environment Variable Check:
;;    Ensures environment variable is set and non-empty
;;    Format: '(env-var VARIABLE-NAME)
;;    Example: '(env-var "HOME")
;;
;; 3. File Content Check:
;;    Verifies if file exists and has content
;;    Format: '(file FILEPATH)
;;    Example: '(file "/tmp/log.txt")
;;
;; 4. Directory Content Check:
;;    Verifies if directory exists and contains files
;;    Format: '(directory DIRPATH)
;;    Example: '(directory "/tmp/build")
;;
;; Behavior Modifiers:
;; ------------------
;; Two optional keyword arguments can modify the macro's behavior:
;;
;; :error-on-fail (boolean)
;;   When t, signals an error if any check fails
;;   Useful in init.el to prevent Emacs from starting with missing prerequisites
;;   Example:
;;     (prerequisite-check
;;      '(executable "git")
;;      :error-on-fail t)
;;
;; :quiet (boolean)
;;   When t, suppresses warning messages for failed checks
;;   Useful for silent checking in functions
;;   Example:
;;     (prerequisite-check
;;      '(executable "python")
;;      :quiet t)
;;
;; Return Value:
;; ------------
;; - Returns t if all checks pass
;; - Returns nil if any check fails (unless :error-on-fail is t)
;; - By default, outputs warnings for failed checks via `warn'
;; - With :error-on-fail t, signals detailed error message on failure
;;
;; Note: All functions prefixed with `preq--` are internal
;; and should not be used directly.

;;; Code:

(defun preq--executable (executable &optional min-version)
  "Check if EXECUTABLE exists and optionally verify its MIN-VERSION.
Returns t if the executable exists and meets version requirements, nil otherwise.
MIN-VERSION should be a string in format 'X.Y.Z'."
  (when-let ((exec-path (executable-find executable)))
    (if min-version
        (let* ((version-output (shell-command-to-string (format "%s --version" executable)))
               (version-match (string-match "[0-9]+\\.[0-9]+\\.[0-9]+" version-output))
               (current-version (when version-match
                                  (match-string 0 version-output))))
          (and current-version
               (version<= min-version current-version)))
      t)))

(defun preq--env-var (var-name)
  "Check if environment variable VAR-NAME is set and non-empty.
Returns the value if set and non-empty, nil otherwise."
  (let ((value (getenv var-name)))
    (and value
         (not (string-empty-p value))
         value)))

(defun preq--file-has-content-p (filepath)
  "Check if FILEPATH exists and has content.
Returns t if file exists and has size > 0, nil otherwise."
  (and (file-regular-p filepath)
       (> (file-attribute-size (file-attributes filepath)) 0)))

(defun preq--dir-has-files-p (dirpath)
  "Check if DIRPATH exists and contains files.
Returns t if directory exists and contains files, nil otherwise."
  (and (file-directory-p dirpath)
       (directory-files dirpath nil directory-files-no-dot-files-regexp)))

(defmacro prerequisite-check (&rest prerequisites)
  "Check multiple prerequisites with custom error messages."
  (let ((checks ())
        (error-on-fail nil)
        (quiet nil))
    (while prerequisites
      (let ((item (pop prerequisites)))
        (cond
         ((eq item :error-on-fail) (setq error-on-fail (pop prerequisites)))
         ((eq item :quiet)         (setq quiet (pop prerequisites)))
         ((consp item)             (push item checks))
         (t (error "Invalid prerequisite item: %S" item)))))
    (setq checks (nreverse checks))

    `(let ((all-passed t)
           (failed-messages ()))
       (cl-block nil
         ,@(cl-mapcar
            (lambda (check)
              (let* ((type (car check))
                     (args (cdr check))
                     (msg  (or (plist-get args :error-msg)
                               (pcase type
                                 (`executable (format "Missing executable: %s" (car args)))
                                 (`env-var    (format "Env var not set: %s" (car args)))
                                 (`file-empty (format "File not empty: %s" (car args)))
                                 (`directory-empty (format "Directory not empty: %s" (car args)))))))
                (pcase type
                  (`executable
                   `(unless (preq--executable ,(car args) ,(plist-get args :min-version))
                      (setq all-passed nil)
                      (push ,msg failed-messages)
                      ,(when error-on-fail `(cl-return nil))))
                  (`env-var
                   `(unless (preq--env-var ,(car args))
                      (setq all-passed nil)
                      (push ,msg failed-messages)
                      ,(when error-on-fail `(cl-return nil))))
                  (`file
                   `(unless (preq--file-has-content-p ,(car args))
                      (setq all-passed nil)
                      (push ,msg failed-messages)
                      ,(when error-on-fail `(cl-return nil))))
                  (`directory
                   `(unless (preq--dir-has-files-p ,(car args))
                      (setq all-passed nil)
                      (push ,msg failed-messages)
                      ,(when error-on-fail `(cl-return nil)))))))
            checks))

       ,(unless quiet
          `(when failed-messages
             (dolist (msg (reverse failed-messages))
               (warn "%s" msg))))

       ,(when error-on-fail
          `(when (not all-passed)
             (error "Prerequisite checks failed: %s"
                    (string-join (reverse failed-messages) "; "))))

       all-passed)))

(provide 'prerequisite-check)
;;; prerequisite-check.el ends here
