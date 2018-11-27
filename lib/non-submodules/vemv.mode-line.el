(provide 'vemv.mode-line)

(defun vemv.abbreviate-ns/format-intermediate-fragment (x)
  (condition-case
      nil (let* ((split (s-split "-" x))
                 (y (mapcar (lambda (f)
                              (substring f 0 1))
                            split)))
            (s-join "-" y))
    (error "")))

(defun vemv/abbreviate-ns (namespace)
  (let* ((split (s-split "\\." (if (vemv/contains? vemv/project-ns-prefix ".")
                                   (s-replace (s-replace ".."
                                                         ""
                                                         (concat vemv/project-ns-prefix "."))
                                              "" namespace)
                                 namespace)))
         (name (car (last split)))
         (bbase (-remove (lambda (x)
                           (string-equal x vemv/project-ns-prefix))
                         (butlast split)))
         (fname (car bbase))
         (base (rest bbase))
         (onechars (mapcar (lambda (x)
                             (vemv.abbreviate-ns/format-intermediate-fragment x))
                           base)))
    (concat fname
            (if fname "." "")
            (s-join "." onechars)
            (if (> (length onechars) 0)
                "."
              "")
            name)))

(defun vemv/mode-line-for-buffer (buffer-filename)
  (let* ((buf (get-file-buffer buffer-filename))
         (buffer-name (buffer-name buf))
         (is-project-dot-clj (vemv/contains? buffer-name "project.clj"))
         (is-clj (vemv/contains? buffer-name ".clj"))
         (is-rails-view (or (vemv/contains? buffer-filename ".haml")
                            (vemv/contains? buffer-filename ".erb")))
         (sym (intern (concat "vemv/mode-line-for-buffer/" buffer-filename "-open")))
         (close-sym (intern (concat "vemv/mode-line-for-buffer/" buffer-filename "-close")))
         (namespace (if is-project-dot-clj
                        "project.clj"
                      (if is-clj
                          (vemv/abbreviate-ns (with-current-buffer buffer-name
                                                (or (ignore-errors
                                                      (cider-current-ns))
                                                    buffer-name))))))
         (is-modified (with-current-buffer buffer-name
                        (buffer-modified-p)))
         (shortname (concat (if is-clj
                                namespace
                              (if is-rails-view
                                  (s-join "/" (-take-last 2 (s-split "/" buffer-filename)))
                                buffer-name))
                            (if is-modified "*" ""))))
    (unless (fboundp sym)
      (eval `(defun ,sym ()
               (interactive)
               (vemv/safe-select-window vemv/main_window)
               (switch-to-buffer (get-file-buffer ,buffer-filename))
               (vemv/clean-chosen-file-buffer-order)
               (vemv/after-file-open)))
      (eval `(defun ,close-sym ()
               (interactive)
               (kill-buffer (get-file-buffer ,buffer-filename))
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

(defun vemv/present-one-tab-per-project-file ()
  (if (not (vemv/buffer-of-current-project? (current-buffer)))
      (buffer-name)
    (if-let* ((all (->> (gethash vemv/current-project vemv/chosen-file-buffer-order)
                        (filter 'identity)
                        (mapcar 'vemv/mode-line-for-buffer))))
        (vemv/format-tabs (car all) (cdr all))
      (buffer-name))))

(defun vemv/pe/mode-line-format* ()
  (->> (vemv/all-project-names :no-prettify)
       cdr
       (mapcar 'vemv/mode-line-for-project)
       (vemv/format-tabs (cider-project-name vemv/current-project))
       (concat "  ")))
