;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(require 'es-lib)
(require 'es-windows)
(require 'project-explorer)
(provide 'vemv.project-explorer)

(setq vemv/project-explorer-initialized nil)

(defun vemv/refresh-pe-cache (&optional done)
  (let ((pe/project-root (funcall pe/project-root-function)))
    (with-selected-window vemv/project-explorer-window
      (with-current-buffer (window-buffer vemv/project-explorer-window)
        (when pe/cache-enabled
          (call-interactively 'pe/cache-clear))
        (funcall pe/directory-tree-function
                 (funcall pe/project-root-function)
                 (lambda (x)
                   (with-selected-window vemv/project-explorer-window
                     (with-current-buffer (window-buffer vemv/project-explorer-window)
                       (setq vemv/project-explorer-initialized t)
                       (pe/set-tree (current-buffer) 'refresh x)
                       (when done
                         (funcall done))))))))))

(setq vemv/refreshing-caches nil)

(defun vemv/timestamp-lock-acquired? (timestamp)
  (and timestamp (< (- (vemv/timestamp) timestamp) 30)))

(defun vemv/refresh-file-caches (&optional done force)
  (if (or (and (not force)
               (vemv/timestamp-lock-acquired? vemv/refreshing-caches))
          (minibuffer-prompt))
      (when done
        (funcall done))
    (setq vemv/refreshing-caches (vemv/timestamp))
    (fiplr-clear-cache)
    (vemv/refresh-pe-cache done)))

(defun vemv/safely-open-pe-window ()
  (when (boundp 'vemv/project-explorer-window)
    (vemv/safe-select-window vemv/project-explorer-window)))

(defun vemv/ensure-project-is-displayed! (done-fn)
  (with-selected-window vemv/project-explorer-window
    (with-current-buffer (window-buffer vemv/project-explorer-window)
      (let* ((expected vemv/project-root-dir)
             (actual (funcall pe/project-root-function))
             (default-directory expected)
             (pe/project-root expected))
        (if (string-equal expected actual)
            (-some-> done-fn funcall)
          (project-explorer-open (argless
                                  (and done-fn (funcall done-fn))
                                  (setq vemv/project-explorer-initialized t))))))))

(defun vemv/main-window-buffer-filename (original-window)
  (let* ((wb (window-buffer (or original-window vemv/main_window))))
    (with-current-buffer wb
      (and (buffer-file-name)
           (file-truename (buffer-file-name))))))

(defun vemv.project-explorer/filename-subsegments (filename)
  (let* ((projname (funcall pe/project-root-function))
         (project-fragments (-remove 's-blank?
                                     (split-string projname "/")))
         (fragments (->> (split-string filename "/")
                         (-remove 's-blank?)
                         (-drop (length project-fragments))))
         (expanded-fragments (mapcar* '-take
                                      (number-sequence 1 (length fragments))
                                      (-repeat (length fragments) fragments))))
    (mapcar (lambda (x)
              (concat (->> x
                           (-interpose "/")
                           (cons projname)
                           (s-join ""))
                      "/"))
            expanded-fragments)))

(setq vemv.project.reasonable-file-count?/threshold 3000)

;; `pe/fold` has some scalability limits, hindering `vemv/show-current-file-in-project-explorer-impl`
(defun vemv.project/reasonable-file-count? ()
  (let* ((command (concat "tree -fi " vemv/project-root-dir " -I " pe/omit-regex " | wc -l"))
         (result (vemv/shell-command-to-tuple command)) ;; extra care here because reusing an Elisp regex (pe/omit-regex) in bash is risky
         (return-code (car result))
         (output (second result)))
    (if (not (zero? return-code))
        (progn
          (vemv/echo "vemv.project/reasonable-file-count? command returned non-zero code: " output)
          nil)
      (let* ((count (-some->> output
                              (s-match "[0-9]+")
                              (car)
                              (string-to-number))))
        (or (-some->> count (> vemv.project.reasonable-file-count?/threshold))
            (progn
              (message (vemv/force-concat "vemv.project.reasonable-file-count?/threshold surpassed: " count ". Highlighting in project-explorer disabled accordingly. General performance might suffer in any case."))
              nil))))))

(defun vemv/show-current-file-in-project-explorer-impl (original-window)
  (let ((buffer-truename (vemv/main-window-buffer-filename original-window)))
    (when (and buffer-truename
               (vemv/contains? buffer-truename vemv/project-root-dir)
               (vemv.project/reasonable-file-count?))
      (let* ((final-fragments (vemv.project-explorer/filename-subsegments buffer-truename))
             (goal (->> final-fragments last first (s-chop-suffix "/"))))

        (with-selected-window vemv/project-explorer-window
          (setq-local cursor-type nil)
          ;; somewhat expensive call. But a well-tuned `pe/omit-regex' will make it acceptable.
          ;; Note: do not comment out anymore - implementation will fail.
          (pe/fold-all)
          (beginning-of-buffer)

          (seq-doseq (f (butlast final-fragments))
            (while (not (string-equal f (pe/current-directory)))
              (next-line))
            (pe/return))

          (while (not (string-equal goal (pe/get-filename)))
            (next-line))

          (beginning-of-line)

          (global-hl-line-highlight))))))

(defun vemv/show-current-file-in-project-explorer-unsafe (original-window)
  (interactive)
  (let ((fallback (argless
                   (funcall vemv/safe-show-current-file-in-project-explorer))))
    (if (minibuffer-prompt)
        (delay fallback 1)
      (vemv/refresh-file-caches (argless (if (minibuffer-prompt)
                                             (delay fallback 1)
                                           (vemv/ensure-project-is-displayed! (lambda (&rest _)
                                                                                (vemv/show-current-file-in-project-explorer-impl original-window)))
                                           (vemv/safe-select-window original-window)))))))

(defun vemv/safe-show-current-file-in-project-explorer* (&optional original-window)
  (when (-some-> (vemv/main-window-buffer-filename original-window) file-exists-p)
    (let* ((w (or original-window (selected-window)))
           (attempts 7)
           (impl (lambda (self attempt-no)
                   (when (pos? attempt-no)
                     (condition-case nil
                         (vemv/show-current-file-in-project-explorer-unsafe w)
                       (error
                        (funcall self self (dec attempts))))))))
      (funcall impl impl attempts))))

(defvar vemv/safe-show-current-file-in-project-explorer
  (vemv/debounce 'vemv/safe-show-current-file-in-project-explorer* 0.2))
