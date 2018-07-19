;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.project-interaction)

(defun vemv/dir-for-project (which)
  (concat vemv-home "/" which))

(defun vemv/projects-with-initialization-files ()
  (if-let (x (car (filter (lambda (x) (vemv/contains? x ".emacs.d.overrides")) load-path)))
      (mapcar
       (lambda (x)
         (s-replace ".el" "" (s-replace "vemv.project." "" x)))
       (-remove (lambda (x)
                  (member x (list "." ".." "emacs.d.overrides.el")))
                (directory-files x)))))

(defun vemv/open-project ()
  (interactive)
  (load "emacs.d.overrides")
  (vemv/set-available-projects!)
  (vemv/save-window-excursion
   (let* ((chosen-workspace (ido-completing-read "In which workspace should the project be opened? " (vemv/workspace-names)))
          (_ (assert (member chosen-workspace (vemv/workspace-names))))
          (default-directory (vemv/dir-opened-from-home))
          (project-name (or (car (filter (lambda (x)
                                           (let ((dfp (vemv/dir-for-project x)))
                                             (or (vemv/contains? default-directory dfp)
                                                 (vemv/contains? dfp default-directory))))
                                         (vemv/projects-with-initialization-files)))
                            default-directory)))
     (conj! vemv/on-the-fly-projects project-name)
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

(defun vemv/buffer-of-current-project? (b)
  (when (and b (buffer-file-name b))
    (vemv/contains? (file-truename (buffer-file-name b))
                    vemv/project-root-dir)))

(defun vemv/buffer-of-current-running-project? (b)
  (when (and b (buffer-file-name b))
    (vemv/contains? (file-truename (buffer-file-name b))
                    vemv/running-project-root-dir)))

(defun vemv/open_file_buffers ()
  (let* ((bs (filter (lambda (x)
                       (vemv/buffer-of-current-project? x))
                     (buffer-list)))
         (c (mapcar (lambda (x) (buffer-name x)) bs)))
    c))

(setq vemv/chosen-file-buffer-order (vemv/hash-map))

(defun vemv/clean-chosen-file-buffer-order ()
  "Removes closed buffers from vemv/chosen-file-buffer-order"
  (let* ((curr (buffer-name (current-buffer)))
         (actually-open (vemv/open_file_buffers))
         (all (-distinct (-concat (gethash vemv/current-project vemv/chosen-file-buffer-order) actually-open)))
         (all-without-curr (-remove (lambda (x) (string-equal x curr)) all))
         (final (cons curr all-without-curr)))
    (puthash vemv/current-project
             (filter (lambda (x)
                       (member x actually-open))
                     final)
             vemv/chosen-file-buffer-order)))

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
           vemv/chosen-file-buffer-order))

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
    (switch-to-buffer vemv/file-buffer-fallback)))

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
