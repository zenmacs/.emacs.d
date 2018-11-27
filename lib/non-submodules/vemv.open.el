;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.open)

(defun vemv/open-at-project-root ()
  (interactive)
  (let ((default-directory vemv/project-root-dir))
    (call-interactively 'vemv/open)))

(defun vemv/after-file-open-without-project-explorer-highlighting ()
  (interactive)
  (when (and (vemv/in-clojure-mode?)
             (not vemv/ns-shown))
    (vemv/toggle-ns-hiding :after-file-open))
  (vemv/clean-chosen-file-buffer-order)
  (setq-local mode-line-format tabbed-line-format)
  (vemv/advice-nrepl)
  (vemv/ensure-repl-visible))

(defvar vemv.after-file-open/avoid-recursion nil)

(defun vemv/after-file-open-impl (&rest ignore)
  (interactive)
  (unless vemv.after-file-open/avoid-recursion
    (let* ((vemv.after-file-open/avoid-recursion t))
      (when (vemv/buffer-of-current-project-or-parent? (current-buffer))
        (vemv/after-file-open-without-project-explorer-highlighting)
        ;; skip debounced call so `vemv.after-file-open/avoid-recursion` is passed
        (vemv/safe-show-current-file-in-project-explorer* (get-buffer-window))))))

(defun vemv/after-file-open (&rest ignore)
  (interactive)
  (with-selected-window vemv/main_window
    (vemv/after-file-open-impl)))

(defvar vemv/after-file-open-watcher
  (add-hook 'focus-in-hook 'vemv/advice-nrepl))

(defun vemv/open (&optional filepath)
  "Opens a file (from FILEPATH or the user input)."
  (interactive)
  (vemv/safe-select-frame)
  (vemv/safe-select-window vemv/main_window)
  (let* ((default-directory (if (memq major-mode '(typescript-mode js-mode))
                                (file-name-directory (buffer-file-name))
                              (if (vemv/contains? (buffer-file-name) vemv/project-root-dir)
                                  default-directory
                                vemv/project-clojure-dir)))
         (file (buffer-name (or (and filepath (find-file filepath))
                                (ido-find-file)))))) ;; magical let - do not unwrap!
  (replying-yes ;; create intermediate directories
   (save-buffer))
  (vemv/refresh-file-caches (argless
                             (vemv/safe-select-window vemv/main_window))
                            :force))

(defvar vemv.fiplr.cache/all-git-modified-files)

(defun vemv/fiplr (&optional opener)
  (setq vemv.fiplr.cache/all-git-modified-files (vemv/all-git-modified-files))
  (if (vemv/contains? vemv/project-fiplr-dir "/")
      (progn
        (vemv/safe-select-window vemv/main_window)
        (fiplr-find-file-in-directory vemv/project-fiplr-dir fiplr-ignored-globs
                                      (lambda (f)
                                        (when (file-exists-p f)
                                          (funcall (or opener #'find-file) f)))))
    (vemv/echo "`vemv/project-fiplr-dir' was set incorrectly due to an unknown bug. Try reloading the project.")))

(defun vemv/open-file-via-fiplr-then-close-previous-buffer ()
  (interactive)
  (select-window vemv/main_window)
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

(defun vemv/in-a-git-repo? (dir)
  (not (vemv/contains? (shell-command-to-string (concat "cd "dir "; "
                                                        "git rev-parse --show-toplevel"))
                       "ot a git repository")))

(defun vemv/git-file-list-for (grep-options &optional git-command)
  (let* ((default-directory vemv/project-root-dir)
         (git-command (or git-command "git status --porcelain"))
         (command (when (vemv/in-a-git-repo? default-directory)
                    (concat "cd " default-directory "; "
                            "cd $(git rev-parse --show-toplevel); "
                            git-command (when grep-options
                                          (concat " | grep " grep-options " | sed s/^...//"))
                            " | while read line; do echo \"$PWD/$line\"; done"))))
    (-some->> command
              shell-command-to-string
              s-lines
              (-remove 's-blank?))))

(defun vemv/git-staged-files ()
  "The new and modified files, that are in the staging area. Does not include deleted files."
  (vemv/git-file-list-for "\"^M \\|^A\""))

(defun vemv/git-unstaged-files ()
  "The new and modified files, that are not in the staging area. Does not include deleted files."
  (vemv/git-file-list-for "\"^ M \\|^??\""))

(defun vemv/all-git-modified-files ()
  (-concat (vemv/git-staged-files) (vemv/git-unstaged-files)))

(defun vemv/open-git-staged-files ()
  (interactive)
  (mapcar 'vemv/open (vemv/git-staged-files)))

(defun vemv/open-git-unstaged-files ()
  (interactive)
  (mapcar 'vemv/open (vemv/git-unstaged-files)))

(defun vemv/open-all-git-files ()
  (interactive)
  (mapcar 'vemv/open (vemv/all-git-modified-files)))

(defun vemv/open-git-diff-against ()
  (interactive)
  (require 'magit-diff)
  (require 'git-gutter)
  (defsubst git-gutter:show-gutter-p (diffinfos)
    t)
  (defsubst git-gutter:reset-window-margin-p ()
    nil)
  (select-window vemv/main_window)
  (let* ((default-directory vemv/project-root-dir)
         (branch (magit-diff-read-range-or-commit "Branch" vemv.project/default-git-branch))
         (files (->> branch
                     (concat "git diff --name-only ")
                     (vemv/git-file-list-for nil)
                     (-filter 'file-exists-p))))
    (mapcar 'find-file-noselect files)
    ;; disabled until git-gutter reworked
    (comm mapcar (lambda (x)
                   (with-current-buffer (get-file-buffer x)
                     (switch-to-buffer (current-buffer) t t)
                     (setq git-gutter:diff-option branch)
                     (git-gutter-mode)))
          files)
    (vemv/next-file-buffer)))
