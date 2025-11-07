;; my-preq.el --- Utility functions for checking prerequisites

;;; Commentary:

;; This module provides a convenient macro for checking various prerequisites
;; in your Emacs configuration or packages. It's particularly useful in init.el
;; to ensure required tools and environment variables are available before
;; loading specific configurations.
;;
;; Public Interface:
;; ----------------
;; The only public interface is the `my-preq' macro, which accepts
;; multiple prerequisites and optional behavior modifiers.
;;
;; Basic Usage:
;;   (my-preq
;;    (executable "git" "2.0.0")
;;    (env-var "HOME")
;;    (file "/tmp/log.txt")
;;    (directory "/tmp/build"))
;;
;; Available Prerequisites:
;; 1. Executable Check:
;;    Verifies program existence and optional version
;;    Format: '(executable PROGRAM-NAME [MIN-VERSION] [:error-msg MESSAGE])
;;    Example: '(executable "git" "2.0.0" :error-msg "Git is required!")
;;
;; 2. Environment Variable Check:
;;    Ensures environment variable is set and non-empty
;;    Format: '(env-var VARIABLE-NAME [:error-msg MESSAGE])
;;    Example: '(env-var "HOME" :error-msg "HOME environment variable not set!")
;;
;; 3. File Content Check:
;;    Verifies if file exists and has content
;;    Format: '(file FILEPATH [:error-msg MESSAGE])
;;    Example: '(file "/tmp/log.txt" :error-msg "Log file missing or empty!")
;;
;; 4. Directory Content Check:
;;    Verifies if directory exists and contains files
;;    Format: '(directory DIRPATH [:error-msg MESSAGE])
;;    Example: '(directory "/tmp/build" :error-msg "Build directory empty!")
;;
;; Behavior Modifiers:
;; ------------------
;; An optional keyword argument can modify the macro's behavior on failure:
;;
;; :on-fail (symbol)
;;   Specifies the action to take if any check fails. Can be one of:
;;   - 'warn (default): Outputs a warning message for each failed check.
;;   - 'error: Signals an error and stops Emacs loading/evaluation.
;;   - 'silent: Suppresses all messages. An invalid option will raise an error.
;;
;;   Example with 'error':
;;     (my-preq
;;      (executable "git")
;;      :on-fail 'error)
;;
;;   Example with 'silent':
;;     (my-preq
;;      (executable "python")
;;      :on-fail 'silent)
;;
;; Return Value:
;; ------------
;; - Returns t if all checks pass.
;; - If any check fails:
;;   - with :on-fail 'warn (default), prints warnings and returns nil.
;;   - with :on-fail 'error, signals a detailed error.
;;   - with :on-fail 'silent, returns nil without any message.
;;
;; Note: All functions prefixed with `my-preq--` are internal
;; and should not be used directly.

;;; Code:

(defun my-preq--executable (executable &optional min-version)
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

(defun my-preq--env-var (var-name)
  "Check if environment variable VAR-NAME is set and non-empty.
Returns the value if set and non-empty, nil otherwise."
  (let ((value (getenv var-name)))
    (and value
         (not (string-empty-p value))
         value)))

(defun my-preq--file-has-content-p (filepath)
  "Check if FILEPATH exists and has content.
Returns t if file exists and has size > 0, nil otherwise."
  (and (file-regular-p filepath)
       (> (file-attribute-size (file-attributes filepath)) 0)))

(defun my-preq--dir-has-files-p (dirpath)
  "Check if DIRPATH exists and contains files.
Returns t if directory exists and contains files, nil otherwise."
  (and (file-directory-p dirpath)
       (directory-files dirpath nil directory-files-no-dot-files-regexp)))

(defmacro my-preq (&rest prerequisites)
  "Check multiple prerequisites with custom error messages."
  (let ((checks ())
        (on-fail-behavior 'warn))
    (while prerequisites
      (let ((item (pop prerequisites)))
        (cond
         ((eq item :on-fail) (setq on-fail-behavior (pop prerequisites)))
         ((consp item)      (push item checks))
         (t (error "Invalid prerequisite item: %S" item)))))
    (setq checks (nreverse checks))

    (unless (memq on-fail-behavior '(warn error silent))
      (error "Invalid :on-fail behavior: '%S'. Must be one of 'warn, 'error, or 'silent" on-fail-behavior))

    `(let ((all-passed t)
           (failed-messages ()))
       (cl-block nil
         ,@(cl-mapcar
            (lambda (check)
              (let* ((type (car check))
                     (args (cdr check)))
                (pcase type
                  (`executable
                   (let* ((program (car args))
                          (rest-args (cdr args))
                          (min-version (when (stringp (car rest-args))
                                         (car rest-args)))
                          (plist (if min-version (cdr rest-args) rest-args))
                          (error-msg (plist-get plist :error-msg)))
                     `(unless (my-preq--executable ,program ,min-version)
                        (setq all-passed nil)
                        (push ,(or error-msg (format "Missing executable: %s" program)) failed-messages)
                        ,(when (eq on-fail-behavior 'error) `(cl-return nil)))))
                  (`env-var
                   (let* ((var-name (car args))
                          (error-msg (plist-get (cdr args) :error-msg)))
                     `(unless (my-preq--env-var ,var-name)
                        (setq all-passed nil)
                        (push ,(or error-msg (format "Env var not set: %s" var-name)) failed-messages)
                        ,(when (eq on-fail-behavior 'error) `(cl-return nil)))))
                  (`file
                   (let* ((filepath (car args))
                          (error-msg (plist-get (cdr args) :error-msg)))
                     `(unless (my-preq--file-has-content-p ,filepath)
                        (setq all-passed nil)
                        (push ,(or error-msg (format "File not found: %s" filepath)) failed-messages)
                        ,(when (eq on-fail-behavior 'error) `(cl-return nil)))))
                  (`directory
                   (let* ((dirpath (car args))
                          (error-msg (plist-get (cdr args) :error-msg)))
                     `(unless (my-preq--dir-has-files-p ,dirpath)
                        (setq all-passed nil)
                        (push ,(or error-msg (format "Directory not found: %s" dirpath)) failed-messages)
                        ,(when (eq on-fail-behavior 'error) `(cl-return nil))))))))
            checks))

       ,(when (eq on-fail-behavior 'warn)
          `(when failed-messages
             (dolist (msg (reverse failed-messages))
               (warn "%s" msg))))

       ,(when (eq on-fail-behavior 'error)
          `(when (not all-passed)
             (error "Prerequisite checks failed: %s"
                    (string-join (reverse failed-messages) "; "))))

       all-passed)))

(provide 'my-preq)
;;; my-preq.el ends here
