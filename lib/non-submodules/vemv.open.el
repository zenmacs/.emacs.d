(provide 'vemv.open)

(defun vemv/open-at-project-root ()
  (interactive)
  (let ((default-directory vemv/project-root-dir))
    (call-interactively 'vemv/open)))

(defun vemv/after-file-open-without-project-explorer-highlighting ()
  (interactive)
  (vemv/safe-select-window vemv/main_window)
  (when (vemv/buffer-of-current-project? (current-buffer))
    (when (and (vemv/in-clojure-mode?)
               (not vemv/ns-shown))
      (vemv/toggle-ns-hiding :after-file-open))
    (setq-local mode-line-format tabbed-line-format)
    (vemv/advice-nrepl)
    (vemv/ensure-repl-visible)))

(defun vemv/after-file-open (&rest ignore)
  (interactive)
  (vemv/after-file-open-without-project-explorer-highlighting)
  (funcall vemv/safe-show-current-file-in-project-explorer))

(defun vemv/open (&optional filepath)
  "Opens a file (from FILEPATH or the user input)."
  (interactive)
  (vemv/safe-select-window vemv/main_window)
  (let ((file (buffer-name (or (and filepath (find-file filepath))
                               (ido-find-file)))))) ;; magical let - do not unwrap!
  (save-buffer)
  (vemv/refresh-file-caches)
  (vemv/safe-select-window vemv/main_window)
  (vemv/after-file-open-without-project-explorer-highlighting)
  (delay (argless (funcall vemv/safe-show-current-file-in-project-explorer))
         20))

(defun vemv/fiplr (&optional opener)
  (if (vemv/contains? vemv/project-fiplr-dir "/")
      (fiplr-find-file-in-directory vemv/project-fiplr-dir fiplr-ignored-globs (or opener #'find-file))
      (vemv/echo "`vemv/project-fiplr-dir' was set incorrectly due to an unknown bug. Try reloading the project.")))

(defun vemv/open-file-via-fiplr-then-close-previous-buffer ()
  (interactive)
  (setq vemv/previous-buffer (current-buffer))
  (vemv/fiplr (lambda (filename)
                (find-file filename)
                (when (not (eq vemv/previous-buffer (current-buffer)))
                  (kill-buffer vemv/previous-buffer)))))

(defun vemv/dir-opened-from-home ()
  (let ((default-directory vemv-home))
    (replace-regexp-in-string "\\.$" "" (ido-read-directory-name ()))))

(defun vemv/open-recent-file-for-this-project! ()
  (when (boundp 'vemv/main_window)
    (let* ((the-file (when (and (file-readable-p recentf-save-file)
                                (pos? (length recentf-list)))
                       (car (filter (lambda (x)
                                      (and x (vemv/contains? (file-truename x) vemv/project-root-dir)
                                           (not (vemv/contains? (file-truename x) "ido.last"))))
                                    recentf-list))))
           (the-file (if (or (not (vemv/clojure-project?))
                             (and the-file
                                  (file-exists-p the-file) ;; file-truename can make up nonexisting files
                                  (vemv/contains? (file-truename the-file) ;; expand symlinks
                                                  vemv/project-clojure-dir)))
                         the-file
                         vemv/default-clojure-file))
           (the-file (if the-file (file-truename the-file))))
      (when (and the-file (file-exists-p the-file))
        (vemv/open the-file)))))
