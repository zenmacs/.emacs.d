;; -*- lexical-binding: t; -*-

(require 'vemv.lang.core)
(provide 'vemv.lang)

;; elisp gotchas: let vs. let* · last returns a list · "Wrong type argument: commandp" -> forgot interactive

(defun vemv/send (where &optional backward? content)
  "Copy the next sexp (or on non-nil backward? arg, the previous sexp) and its character trailer,
  switch to the window that is assigned for REPL purposes, then it switch to the corresponding buffer
  (different REPLs have different buffers),
  paste and simulate an intro press. Finally, go back to sender window."
  (interactive)

  (let ((content (or content
                     (if (region-active-p)
                         (vemv/selected-region)
                         (vemv/sexpr-content backward?)))))
    (if (equal where :emacs)
        (eval (read content))
        (let ((sender (selected-window))
              (destination-buffer (case where
                                    (:cider the-cider-buffer-name)
                                    (:ielm "*ielm*")
                                    (:shell "*shell-1*")
                                    (:clj vemv/clj-repl-name)
                                    (:cljs vemv/cljs-repl-name))))
          (if (not (seq-contains (vemv/all-buffer-names) destination-buffer))
              (vemv/echo "Can't eval in a different project!")
              (vemv/safe-select-window vemv/repl2)
              (switch-to-buffer destination-buffer)

              (end-of-buffer)
              (insert content)

              (case where
                (:cider (cider-repl-return))
                (:ielm (ielm-return))
                (:shell (comint-send-input))
                (:clj (cider-repl-return))
                (:cljs (cider-repl-return)))

              (pop kill-ring)
              (end-of-buffer))
          (vemv/safe-select-window sender)))))

(defun vemv/open-at-project-root ()
  (interactive)
  (let ((default-directory vemv/project-root-dir))
    (call-interactively 'vemv/open)))

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
    (select-window vemv/repl2)
    (vemv/send :shell nil vemv/project-root-dir)
    (delay (argless
            (comint-clear-buffer)
            (select-window vemv/main_window))
           0.3)))

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
                 `(file ,@(butlast (gethash vemv/current-project vemv/chosen-file-buffer-order)))
                 vemv/chosen-file-buffer-order))
    (switch-to-buffer vemv/file-buffer-fallback)))

(setq vemv/shell-id 0)

(defun sh ()
  (interactive)
  (shell (concat "*shell-" (number-to-string (send! vemv/shell-id (lambda (a) (inc a)))) "*")))

(defun vemv/fiplr (&optional opener)
  (fiplr-find-file-in-directory vemv/project-fiplr-dir fiplr-ignored-globs (or opener #'find-file)))

(defun vemv/save-all-buffers-for-this-project ()
  (mapcar (lambda (b)
            (when (and (vemv/buffer-of-current-project? b))
              (vemv/save b)))
          (vemv/all-buffers)))

(defun vemv/open-file-via-fiplr-then-close-previous-buffer ()
  (interactive)
  (setq vemv/previous-buffer (current-buffer))
  (vemv/fiplr (lambda (filename)
                (find-file filename)
                (when (not (eq vemv/previous-buffer (current-buffer)))
                  (kill-buffer vemv/previous-buffer)))))

(defun vemv/smex ()
  (when vemv/launched (smex)))

(defun vemv/emacs-reload ()
  (let ((was-verbose vemv/verbose-mode))
    (when (not was-verbose)
      (vemv/toggle-verbosity))
    (load "vemv.lang")
    (load "vemv.project")
    (load "vemv.workspace")
    (load "vemv.data.bindings")
    (load "vemv.shortcuts.global")
    (load "vemv.shortcuts.clojure")
    (load "vemv.shortcuts.ruby")
    (load "vemv.theme")
    (when (not was-verbose)
      (vemv/toggle-verbosity))
    (vemv/echo "Reloaded!")))

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
