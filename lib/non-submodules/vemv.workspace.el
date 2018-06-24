(require 'vemv.lang)
(provide 'vemv.workspace)

(defun vemv/find-workspace (name)
  (car (filter (lambda (x)
                 (string-equal (car x) name))
               vemv/all-workspaces)))

(defun vemv/set-workspace (&optional to skip-refresh reverse)
  (let* ((old vemv/current-workspace)
         (which (or to (car vemv/all-workspaces)))
         (name (car which)))
    (when (not (eq old to))
      (setq vemv/current-workspace which)
      (setq vemv/all-workspaces (cons which (-remove (lambda (x)
                                                       (string-equal (car x) name))
                                                     (if reverse
                                                         (cons (car vemv/all-workspaces) (cdr vemv/all-workspaces))
                                                         (-snoc (cdr vemv/all-workspaces) (car vemv/all-workspaces))))))
      (unless skip-refresh
        (vemv/force-refresh-project!)))))

(defun vemv/next-workspace ()
  (vemv/set-workspace (second vemv/all-workspaces)))

(defun vemv/previous-workspace ()
  (vemv/set-workspace (car (last vemv/all-workspaces)) nil :reverse))

(defun vemv/mode-line-for-workspace (workspace-name)
  (let* ((sym (intern (concat "vemv/mode-line-for-workspace/" workspace-name "-open")))
         (close-sym (intern (concat "vemv/mode-line-for-workspace/" workspace-name "-close"))))
    (unless (fboundp sym)
      (eval `(defun ,sym ()
               (interactive)
               (vemv/set-workspace (vemv/find-workspace ,workspace-name))
               (force-mode-line-update))))
    (propertize workspace-name 'local-map `(keymap
                                          (mode-line keymap
                                                     (mouse-1 . ,sym)
                                                     (mouse-3 . ,close-sym))))))

(defun vemv/workspace-mode-line-format ()
  (let* ((first (car vemv/current-workspace))
         (rest (mapcar 'vemv/mode-line-for-workspace (mapcar 'car (cdr vemv/all-workspaces)))))
    (concat "  " (vemv/format-tabs first rest))))

(defun vemv/add-project-to-current-workspace (project)
  (let* ((curr (car vemv/all-workspaces))
         (name (car curr)))
    (setq vemv/all-workspaces
          (cons (list name (cons project (second curr)))
                (cdr vemv/all-workspaces)))))

(defun vemv/refresh-workspace-projects ()
  (let* ((curr (car vemv/all-workspaces))
         (name (car curr)))
    (setq vemv/all-workspaces
          (cons (list name (filter 'vemv/should-show-project? (second curr)))
                (cdr vemv/all-workspaces)))))

(defun vemv/next-project-within-workspace ()
  (let* ((curr (car vemv/all-workspaces))
         (name (car curr))
         (projects (second curr))
         (newval `(,@(cdr projects) ,(car projects))))
    (setq vemv/all-workspaces
          (cons (list name newval)
                (cdr vemv/all-workspaces)))))

(defun vemv/previous-project-within-workspace ()
  (let* ((curr (car vemv/all-workspaces))
         (name (car curr))
         (projects (second curr))
         (newval `(,(or (car (last projects))
                        (first projects))
                   ,@(butlast projects))))
    (setq vemv/all-workspaces
          (cons (list name newval)
                (cdr vemv/all-workspaces)))))

(defun vemv/select-project-within-workspace (project-name)
  (let* ((curr (car vemv/all-workspaces))
         (name (car curr))
         (projects (second curr))
         (newval (cons project-name (-remove (lambda (x)
                                                (string-equal x project-name))
                                              projects))))
    (setq vemv/all-workspaces
          (cons (list name newval)
                (cdr vemv/all-workspaces)))))

(defun vemv/close-project-within-workspace (project-name)
  (if (member project-name vemv/on-the-fly-projects)
      (progn (let* ((curr (car vemv/all-workspaces))
                    (name (car curr))
                    (projects (second curr))
                    (newval (-remove (lambda (x)
                                       (string-equal x project-name))
                                     projects)))
               (setq vemv/all-workspaces
                     (cons (list name newval)
                           (cdr vemv/all-workspaces))))
             t)))

(defun vemv/projects-for-workspace (&optional which)
  (let* ((name (or which (car (car vemv/all-workspaces))))
         (workspace (vemv/find-workspace name)))
    (second workspace)))

(defun vemv/workspace-names ()
  (mapcar 'car vemv/all-workspaces))
