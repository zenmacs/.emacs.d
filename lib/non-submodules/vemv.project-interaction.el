;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.project-interaction)

(defun vemv/dir-for-project (which)
  (concat vemv-home "/" which))

(defun vemv/projects-enabled-in-config ()
  "The projects that are enabled in .emacs.d.overrides.el and were `load`-ed into the system."
  (-flatten (mapcar 'second vemv/available-workspaces)))

(defun vemv/projects-with-initialization-files ()
  (if-let (x (-find (lambda (x)
                      (vemv/contains? x ".emacs.d.overrides"))
                    load-path))
      (->> x
           directory-files
           (-remove (lambda (x)
                      (member x (list "." ".." "emacs.d.overrides.el"))))
           (mapcar (lambda (x)
                     (s-replace ".el" "" (s-replace "vemv.project." "" x)))))))

(defun vemv/projects-from-central-config-or-dedicated-files ()
  "The set of projects that are either defined (and enabled) in .emacs.d.overrides.el,
   or have a dedicated .el file"
  (->> (vemv/projects-enabled-in-config)
       (-concat (vemv/projects-with-initialization-files))
       -uniq))

(defun vemv/open-project ()
  "Can open a project without configuration whatsoever, or a disabled project (in overrides.el) with(out) a dedicated .el file"
  (interactive)
  (load "emacs.d.overrides")
  (vemv/set-available-projects!)
  (vemv/save-window-excursion
   (let* ((chosen-workspace (ido-completing-read "In which workspace should the project be opened? " (vemv/workspace-names)))
          (_ (assert (member chosen-workspace (vemv/workspace-names))))
          (default-directory (vemv/dir-opened-from-home))
          (found (-find (lambda (x)
                          (let ((dfp (vemv/dir-for-project x)))
                            (or (vemv/contains? default-directory dfp)
                                (vemv/contains? dfp default-directory))))
                        (vemv/projects-from-central-config-or-dedicated-files)))
          (project-name (or found default-directory)))
     (assert (not (member default-directory (list "/" vemv-home (vemv/root-marker)))))
     (assert (file-exists-p default-directory))
     (assert (not (member project-name vemv/available-projects)))
     (assert (not (member project-name (vemv/projects-for-workspace))))
     (conj! vemv/on-the-fly-projects (if found
                                         found
                                         default-directory))
     (vemv/set-workspace (vemv/find-workspace chosen-workspace)
                         :skip-refresh)
     (vemv/add-project-to-current-workspace project-name)
     (vemv/force-refresh-project!))))

(defun vemv/maybe-change-project-graphically* ()
  (vemv/next-file-buffer)
  (vemv/previous-file-buffer)
  (select-window vemv/project-explorer-window)
  (let ((default-directory vemv/project-root-dir))
    (call-interactively 'project-explorer-open))
  (unless (or cider-launched vemv-cider-connected (cider-connected-p))
    (select-window vemv/repl-window)
    (if (eq vemv/project-type :elisp)
        (switch-to-buffer "*ielm*")
        (vemv/send :shell nil vemv/project-root-dir)
        (delay (argless
                (comint-clear-buffer)
                (select-window vemv/main_window))
               0.3))))

(setq vemv/maybe-change-project-graphically
      (vemv/debounce 'vemv/maybe-change-project-graphically* 0.3))

(defun vemv/buffer-of-current-project? (b &optional other-candidates)
  (when (and b (buffer-file-name b))
    (let* ((tn (file-truename (buffer-file-name b))))
      (->> other-candidates
           (cons vemv/project-root-dir)
           (-find (lambda (x)
                    (vemv/contains? tn x)))))))

(defun vemv/buffer-of-current-project-or-parent? (b)
  (vemv/buffer-of-current-project? b vemv/parent-project-root-dirs))

(defun vemv/buffer-of-current-running-project? (b &optional candidates)
  (when (and b (buffer-file-name b))
    (let ((tn (file-truename (buffer-file-name b))))
      (->> candidates
           (cons vemv/running-project-root-dir)
           (-find (lambda (x)
                    (vemv/contains? tn x)))))))

(defun vemv/buffer-of-current-running-project-or-children? (b)
  (vemv/buffer-of-current-running-project? b
                                           (when (-find (lambda (x)
                                                          (string-equal x vemv/running-project-root-dir))
                                                        vemv/parent-project-root-dirs)
                                             (list vemv/project-root-dir))))

(defun vemv/open_file_buffers ()
  (->> (buffer-list)
       (filter 'vemv/buffer-of-current-project?)
       (mapcar 'buffer-name)))

(setq vemv/chosen-file-buffer-order (vemv/hash-map))

(setq vemv/chosen-file-buffer-order-as-list nil)

(defun vemv/refresh-chosen-file-buffer-order-as-list! ()
  (ignore-errors
    (setq vemv/chosen-file-buffer-order-as-list
          (mapcar (lambda (e)
                    (let* ((proj (car e))
                           (buffnames (reverse (mapcar (lambda (b)
                                                         (with-current-buffer (get-buffer b)
                                                           (buffer-file-name)))
                                                       (-remove 'nil?
                                                                (second e))))))
                      (list proj buffnames)))
                  (vemv/hash-map-to-list vemv/chosen-file-buffer-order)))))

(defun vemv/clean-chosen-file-buffer-order ()
  "Removes closed buffers from vemv/chosen-file-buffer-order"
  (let* ((curr (buffer-name (current-buffer)))
         (actually-open (vemv/open_file_buffers))
         (all (-distinct (-concat (gethash vemv/current-project vemv/chosen-file-buffer-order) actually-open)))
         (all-without-curr (-remove (lambda (x)
                                      (string-equal x curr))
                                    all))
         (final (cons curr all-without-curr)))
    (puthash vemv/current-project
             (filter (lambda (x)
                       (and x (member x actually-open)))
                     final)
             vemv/chosen-file-buffer-order)
    (vemv/refresh-chosen-file-buffer-order-as-list!)))

(setq vemv/file-buffer-fallback "*scratch*")

(defun vemv/next-file-buffer ()
  "Switch to the next buffer that contains a file opened by the user within this project"
  (interactive)
  (when (vemv/good-frame-p)
    (vemv/safe-select-window vemv/main_window))
  (vemv/clean-chosen-file-buffer-order)
  (switch-to-buffer (let ((entry (gethash vemv/current-project vemv/chosen-file-buffer-order)))
                      (or (second entry)
                          (first entry)
                          vemv/file-buffer-fallback)))
  (puthash vemv/current-project
           `(,@(cdr (gethash vemv/current-project vemv/chosen-file-buffer-order))
             ,(car (gethash vemv/current-project vemv/chosen-file-buffer-order)))
           vemv/chosen-file-buffer-order)
  (vemv/refresh-chosen-file-buffer-order-as-list!))

(defun vemv/previous-file-buffer ()
  "Switch to the previous buffer that contains a file opened by the user within this project"
  (interactive)
  (when (vemv/good-frame-p)
    (vemv/safe-select-window vemv/main_window))
  (vemv/clean-chosen-file-buffer-order)
  (if-let (file (or (car (last (gethash vemv/current-project vemv/chosen-file-buffer-order)))
                    (first (gethash vemv/current-project vemv/chosen-file-buffer-order))))
      (progn
        (switch-to-buffer file)
        (puthash vemv/current-project
                 `(,file ,@(butlast (gethash vemv/current-project vemv/chosen-file-buffer-order)))
                 vemv/chosen-file-buffer-order))
    (switch-to-buffer vemv/file-buffer-fallback))
  (vemv/refresh-chosen-file-buffer-order-as-list!))

(defun vemv/save-all-buffers-for-this-project ()
  (mapcar (lambda (b)
            (when (and (vemv/buffer-of-current-project? b))
              (vemv/save b)))
          (vemv/all-buffers)))

(defun vemv/should-show-project? (x)
  (or (string-equal x vemv/current-project)
      (vemv/starts-with (vemv/root-marker) x)
      (member x vemv/available-projects)
      (member x vemv/on-the-fly-projects)))

(defun vemv/refresh-available-projects ()
  (load "emacs.d.overrides")
  (vemv/set-available-projects!)
  (dolist (workspace vemv/available-workspaces)
    (mapcar (lambda (x)
              (vemv/add-project-to-current-workspace x))
            (-difference (second workspace) (second (vemv/find-workspace (car workspace))))))
  (vemv/refresh-workspace-projects))

(defun vemv/force-refresh-project! ()
  (vemv/refresh-current-project (car (second (car vemv/all-workspaces))) :switch))

(defun vemv/next-project ()
  (interactive)
  (vemv/refresh-available-projects)
  (vemv/next-project-within-workspace)
  (vemv/force-refresh-project!))

(defun vemv/previous-project ()
  (interactive)
  (vemv/refresh-available-projects)
  (vemv/previous-project-within-workspace)
  (vemv/force-refresh-project!))

(defun vemv/clojure-project? ()
  (vemv/contains? (vemv/keyword-to-string vemv/project-type) "clj"))
