(provide 'vemv.mode-line)

(defun vemv.abbreviate-ns/format-intermediate-fragment (x)
  (condition-case
      nil (let* ((split (s-split "-" x))
                 (y (mapcar (lambda (f) (substring f 0 1)) split)))
            (s-join "-" y))
    (error "")))

(defun vemv/abbreviate-ns (namespace)
  (let* ((split (s-split "\\." namespace))
         (name (car (last split)))
         (bbase (-remove (lambda (x) (string-equal x vemv/project-ns-prefix)) (butlast split)))
         (fname (car bbase))
         (base (rest bbase))
         (onechars (mapcar (lambda (x)
                             (vemv.abbreviate-ns/format-intermediate-fragment x))
                           base)))
    (concat fname (if fname "." "") (s-join "." onechars) (if (> (length onechars) 0) "." "") name)))

(defun vemv/mode-line-for-buffer (buffer-name)
  (let* ((is-project-dot-clj (vemv/contains? buffer-name "project.clj"))
         (is-clj (vemv/contains? buffer-name ".clj"))
         (buf (get-buffer buffer-name))
         (sym (intern (concat "vemv/mode-line-for-buffer/" (buffer-file-name buf) "-open")))
         (close-sym (intern (concat "vemv/mode-line-for-buffer/" (buffer-file-name buf) "-close")))
         (namespace (if is-project-dot-clj
                        "project.clj"
                        (if is-clj
                            (vemv/abbreviate-ns (with-current-buffer buffer-name
                                                  (or (ignore-errors
                                                        (cider-current-ns))
                                                      buffer-name))))))
         (is-modified (with-current-buffer buffer-name (buffer-modified-p)))
         (shortname (concat (if is-clj namespace buffer-name)
                            (if is-modified "*" ""))))
    (unless (fboundp sym)
      (eval `(defun ,sym ()
               (interactive)
               ()
               (vemv/safe-select-window vemv/main_window)
               (switch-to-buffer ,buffer-name)
               (vemv/clean-chosen-file-buffer-order)
               (vemv/after-file-open)))
      (eval `(defun ,close-sym ()
               (interactive)
               (kill-buffer ,buffer-name)
               (vemv/clean-chosen-file-buffer-order))))
    (propertize shortname 'local-map `(keymap
                                       (mode-line keymap
                                                  (mouse-1 . ,sym)
                                                  (mouse-3 . ,close-sym))))))

(defun vemv/mode-line-for-project (project-name)
  (let* ((sym (intern (concat "vemv/mode-line-for-project/" project-name "-open")))
         (close-sym (intern (concat "vemv/mode-line-for-project/" project-name "-close"))))
    (unless (fboundp sym)
      (eval `(defun ,sym ()
               (interactive)
               (vemv/select-project-within-workspace ,project-name)
               (vemv/force-refresh-project!)))
      (eval `(defun ,close-sym ()
               (interactive)
               (when (vemv/close-project-within-workspace ,project-name)
                 (vemv/force-refresh-project!)))))
    (propertize (cider-project-name project-name)
                'local-map
                `(keymap
                  (mode-line keymap
                             (mouse-1 . ,sym)
                             (mouse-3 . ,close-sym))))))

(defun vemv/format-tabs (first rest)
  (let* ((p (propertize first 'face 'font-lock-function-name-face))
         (sep (propertize " | " 'face 'font-lock-line-and-column-face))
         (all (cons p rest)))
    (apply 'concat (-interpose sep all))))

(defun vemv/message-file-buffers-impl ()
  (if (not (vemv/buffer-of-current-project? (current-buffer)))
      (buffer-name)
      (let* ((x (car (gethash vemv/current-project vemv/chosen-file-buffer-order)))
             (first (if (vemv/contains? x ".clj")
                        (if (vemv/contains? x "project.clj")
                            x
                            (vemv/abbreviate-ns (or (ignore-errors (cider-current-ns))
                                                    x)))
                        x))
             (first (when x
                      (with-current-buffer (get-buffer x)
                        (if (buffer-modified-p)
                            (concat first "*")
                            first))))
             (rest (mapcar 'vemv/mode-line-for-buffer (cdr (gethash vemv/current-project vemv/chosen-file-buffer-order)))))
        (vemv/format-tabs first rest))))

(defun vemv/pe/mode-line-format* ()
  (let* ((first (cider-project-name vemv/current-project))
         (rest (mapcar 'vemv/mode-line-for-project (cdr (vemv/all-project-names :no-prettify)))))
    (concat "  " (vemv/format-tabs first rest))))
