;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.project-explorer)

(setq vemv/project-explorer-initialized nil)

(defun vemv/refresh-pe-cache (&optional done)
  (let ((pe/project-root (funcall pe/project-root-function)))
    (vemv/safe-select-window vemv/project-explorer-window)
    (when pe/cache-enabled
      (call-interactively 'pe/cache-clear))
    (with-current-buffer (window-buffer vemv/project-explorer-window)
      (funcall pe/directory-tree-function
               (funcall pe/project-root-function)
               (lambda (x)
                 (vemv/safe-select-window vemv/project-explorer-window)
                 (setq vemv/project-explorer-initialized t)
                 (with-current-buffer (window-buffer vemv/project-explorer-window)
                   (pe/set-tree (current-buffer) 'refresh x)
                   (when done
                     (funcall done))))))))

(setq vemv/refreshing-caches
      nil)

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
  (vemv/safe-select-window vemv/project-explorer-window)
  (let* ((expected vemv/project-root-dir)
         (actual (funcall pe/project-root-function))
         (default-directory expected)
         (pe/project-root expected))
    (if (string-equal expected actual)
        (and done-fn (funcall done-fn))
      (with-current-buffer (window-buffer vemv/project-explorer-window)
        (project-explorer-open (argless
                                (and done-fn (funcall done-fn))
                                (setq vemv/project-explorer-initialized t)))))))

(defun vemv/show-current-file-in-project-explorer-impl ()
  (let ((buffer-truename (with-current-buffer (window-buffer vemv/main_window)
                           (and (buffer-file-name)
                                (file-truename (buffer-file-name))))))
    (when (and buffer-truename (vemv/contains? buffer-truename vemv/project-root-dir))
      (let* ((buffer-fragments (-remove (lambda (x)
                                          (string-equal x ""))
                                        (split-string buffer-truename "/")))
             (projname (funcall pe/project-root-function))
             (project-fragments (-remove (lambda (x)
                                           (string-equal x ""))
                                         (split-string projname "/")))
             (fragments (-drop (length project-fragments)
                               buffer-fragments))
             (expanded-fragments (mapcar* (lambda (x y)
                                            (-take x y))
                                          (number-sequence 1 (length fragments)) (-repeat (length fragments) fragments)))
             (final-fragments (mapcar (lambda (x)
                                        (concat (s-join "" (cons projname (-interpose "/" x))) "/"))
                                      expanded-fragments)))

        (vemv/safe-select-window vemv/project-explorer-window)
        ;; somewhat expensive call. But a well-tuned `pe/omit-regex' will make it acceptable.
        ;; Note: do not comment out anymore - implementation will fail.
        (pe/fold-all)
        (beginning-of-buffer)

        (seq-doseq (f (butlast final-fragments))
          (while (not (string-equal f (pe/current-directory)))
            (next-line))
          (pe/return))

        (while (not (string-equal (->> final-fragments last first (s-chop-suffix "/"))
                                  (pe/get-filename)))
          (next-line))

        (end-of-line)))))

(defun vemv/show-current-file-in-project-explorer-unsafe (original-window)
  (interactive)
  (let ((fallback (argless
                   (funcall vemv/safe-show-current-file-in-project-explorer))))
    (if (minibuffer-prompt)
        (delay fallback 1)

      (vemv/refresh-file-caches (argless (if (minibuffer-prompt)
                                             (delay fallback 1)
                                           (vemv/ensure-project-is-displayed! 'vemv/show-current-file-in-project-explorer-impl))
                                         (vemv/safe-select-window original-window))))))

(defun vemv/safe-show-current-file-in-project-explorer* ()
  (let* ((w (selected-window))
         (attempts 7)
         (impl (lambda (self attempt-no )
                 (when (pos? attempt-no)
                   (condition-case nil
                       (vemv/show-current-file-in-project-explorer-unsafe w)
                     (funcall self self (dec attempts)))))))
    (funcall impl impl attempts)))

(defvar vemv/safe-show-current-file-in-project-explorer
  (vemv/debounce 'vemv/safe-show-current-file-in-project-explorer* 0.8))
