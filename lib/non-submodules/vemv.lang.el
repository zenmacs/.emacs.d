;; -*- lexical-binding: t; -*-

(require 'vemv.lang.core)
(provide 'vemv.lang)

;; elisp gotchas: let vs. let* · last returns a list · "Wrong type argument: commandp" -> forgot interactive

(defun vemv/bounded-list/insert-at-head! (x bounded-list bound)
  (vemv/mutate-list-to bounded-list (cons x (-clone bounded-list)))
  (vemv/mutate-list-to bounded-list (-take bound (-clone bounded-list)))
  bounded-list)

(defun vemv/bounded-list/insert-at-second-position! (x bounded-list bound)
  (let ((head (car bounded-list)))
    (vemv/mutate-list-to bounded-list (rest (-clone bounded-list)))
    (vemv/mutate-list-to bounded-list (cons x (-clone bounded-list)))
    (vemv/mutate-list-to bounded-list (cons head (-clone bounded-list)))
    (vemv/mutate-list-to bounded-list (-take bound (-clone bounded-list)))
    bounded-list))

(setq vemv/kill-list-bound 10)

;; The 10 last elements copied to the clipboard.
;; I don't use kill-ring, since third-parties (e.g. paredit) can mess with it
(setq vemv/kill-list (-repeat vemv/kill-list-bound nil))

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

(setq vemv/line-before-formatting nil)
(setq vemv/token-before-formatting nil)

(defun vemv/save-position-before-formatting ()
  (setq vemv/line-before-formatting (max 0 (- (vemv/current-line-number) 3)))
  (setq vemv/token-before-formatting (vemv/sexpr-content)))

;; XXX unused
;; XXX should be per-buffer (see vemv/save-all-clojure-buffers-for-this-project)
(defun vemv/restore-position-before-formatting ()
  (beginning-of-buffer)
  (dotimes (i vemv/line-before-formatting)
    (next-line))
  (ignore-errors (search-forward vemv/token-before-formatting))
  (paredit-backward)
  (back-to-indentation))

(defun vemv/save (&optional b)
  (interactive)
  (let ((b (or b (current-buffer))))
    (with-current-buffer b
      (unless (or (eq clojure-indent-style :align-arguments) clojure-align-forms-automatically)
        (when (vemv/ciderable-p)
          (vemv/save-position-before-formatting)
          (let ((old (substring-no-properties (buffer-string))))
            (save-excursion
              (condition-case nil (cider-format-buffer)
                (error
                 (erase-buffer)
                 (insert old)))))))
      (save-buffer))))

(defun vemv/save-all-clojure-buffers-for-this-project () ;; XXX make not clojure-only
  (mapcar (lambda (b)
            (when (and (vemv/contains? (buffer-name b) ".clj")
                       (vemv/buffer-of-current-project? b))
              (vemv/save b)))
          (vemv/all-buffers)))

(defun vemv/tab ()
  (interactive)
  (or (and (or
            (vemv/in-indentation-point-p)
            (vemv/non-completable-char-p))
           (or (call-interactively 'indent-for-tab-command)
               t))
      (call-interactively 'company-complete)
      (call-interactively 'company-dabbrev)))


(defun vemv/open-file-via-fiplr-then-close-previous-buffer ()
  (interactive)
  (setq vemv/previous-buffer (current-buffer))
  (vemv/fiplr (lambda (filename)
                (find-file filename)
                (when (not (eq vemv/previous-buffer (current-buffer)))
                  (kill-buffer vemv/previous-buffer)))))

(defun vemv/smex ()
  (when vemv/launched (smex)))

(defun vemv/cut ()
  (interactive)
  (vemv/bounded-list/insert-at-head! (vemv/kill nil nil)
                                     vemv/kill-list
                                     vemv/kill-list-bound))

(defun vemv/copy-inserting-at-kill-list ()
  (interactive)
  (vemv/bounded-list/insert-at-head! (vemv/copy-selection-or-next-sexpr)
                                     vemv/kill-list
                                     vemv/kill-list-bound))

(defun vemv/maybe-indent-on-paste (content)
  (when (and (vemv/in-a-lisp-mode?)
             (s-match "^\s*[\(|[|{]" content))
    (paredit-backward)
    (vemv/indent)))

(defun vemv/paste-from-clipboard ()
  (let ((content (substring-no-properties (simpleclip-get-contents))))
    (insert content)
    (vemv/maybe-indent-on-paste content)))

(defun vemv/paste-from-kill-list ()
  (let ((content (car vemv/kill-list)))
    (insert content)
    (vemv/maybe-indent-on-paste content)))

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

(defun vemv/replace-regexp-entire-buffer (pattern replacement)
  "Perform regular-expression replacement throughout buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))
