;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.project-explorer)

(defun vemv/refresh-pe-cache ()
  (vemv/safe-select-window vemv/project-explorer-window)
  (call-interactively 'pe/cache-clear)
  (funcall pe/directory-tree-function
           default-directory
           (apply-partially 'pe/set-tree (current-buffer) 'refresh))
  (vemv/safe-select-window vemv/main_window))

(setq vemv/refreshing-caches
      nil)

(setq vemv/project-explorer-initialized
      nil)

(defun vemv/timestamp-lock-acquired? (timestamp)
  (and timestamp (< (- (vemv/timestamp) timestamp) 30)))

(defun vemv/refresh-file-caches ()
  (unless (or (vemv/timestamp-lock-acquired? vemv/refreshing-caches)
              (not vemv/project-explorer-initialized)
              (minibuffer-prompt)
              (not (vemv/contains? (buffer-name (current-buffer)) ".clj")))
    (setq vemv/refreshing-caches (vemv/timestamp))
    (vemv/refresh-pe-cache)
    (fiplr-clear-cache)))

(defun vemv/safely-open-pe-window ()
  (when (boundp 'vemv/project-explorer-window)
    (vemv/safe-select-window vemv/project-explorer-window)))


(defun vemv/ensure-project-is-displayed! ()
  (vemv/save-window-excursion
   (vemv/safe-select-window vemv/project-explorer-window)
   (let* ((expected vemv/project-root-dir)
          (actual (pe/project-root-function-default))
          (default-directory expected))
     (when (not (string-equal expected actual))
       (call-interactively 'project-explorer-open)))))

(defun vemv/show-current-file-in-project-explorer-unsafe ()
  (interactive)
  (let ((fallback (argless (funcall vemv/safe-show-current-file-in-project-explorer))))
    (if (minibuffer-prompt)
        (delay fallback 1)

        (vemv/refresh-file-caches)
        (vemv/safe-select-window vemv/main_window)
        (if (minibuffer-prompt)
            (delay fallback 1)

            (vemv/ensure-project-is-displayed!)
            (let ((buffer-truename (file-truename (buffer-file-name))))
              (when (vemv/contains? buffer-truename vemv/project-root-dir)
                (let* ((buffer-fragments (-remove (lambda (x) (string-equal x "")) (split-string buffer-truename "/")))
                       (projname (pe/project-root-function-default)) ;; "/Users/vemv/gpm"
                       (project-fragments (-remove (lambda (x) (string-equal x "")) (split-string projname "/")))
                       (fragments (-drop (length project-fragments) buffer-fragments))
                       (expanded-fragments (mapcar* (lambda (x y)
                                                      (-take x y))
                                                    (number-sequence 1 (length fragments)) (-repeat (length fragments) fragments)))
                       (final-fragments (mapcar (lambda (x)
                                                  (concat (s-join "" (cons projname (-interpose "/" x))) "/"))
                                                expanded-fragments)))

                  (vemv/safe-select-window vemv/project-explorer-window)
                  ;; (pe/fold-all) ;; necessary in principle, skip it for performance. seems to work fine.
                  (beginning-of-buffer)

                  (seq-doseq (f (butlast final-fragments))
                    (while (not (string-equal f (pe/current-directory)))
                      (next-line))
                    (pe/return))

                  (while (not (string-equal (s-chop-suffix "/" (first (last final-fragments))) (pe/get-filename)))
                    (next-line))

                  (end-of-line))))))))

(defun vemv/safe-show-current-file-in-project-explorer* ()
  (condition-case nil
      (vemv/show-current-file-in-project-explorer-unsafe)
    (error (ignore-errors (vemv/show-current-file-in-project-explorer-unsafe))))
  (vemv/safe-select-window vemv/main_window))

(setq vemv/safe-show-current-file-in-project-explorer
      (vemv/debounce 'vemv/safe-show-current-file-in-project-explorer* 0.8))
