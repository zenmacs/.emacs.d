;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.open)

(defun vemv/jump-to-first-git-modified-line ()
  (interactive)
  (when (and (not buffer-read-only)
             (not (equal major-mode 'image-mode))
             (equal 1 (vemv/current-line-number))
             (buffer-file-name))
    (let* ((line (condition-case nil
                     (read (shell-command-to-string (concat "cd " default-directory  "; ~/.emacs.d/scripts/first_diff_line.sh " (buffer-file-name))))
                   (error
                    nil))))
      (when line
        (condition-case nil
            (goto-line line)
          (error
           nil))))))

(defun vemv/open-at-project-root ()
  (interactive)
  (let ((default-directory vemv/project-root-dir))
    (call-interactively 'vemv/open)))

(defun vemv/after-file-open-without-project-explorer-highlighting (of-current-project?)
  (interactive)

  (when of-current-project?
    (let* ((t? (cljr--in-tests-p)))
      (when (and (vemv/in-clojure-mode?)
                 (not vemv/ns-shown)
                 (or (not t?)
                     (and t?
                          (not (buffer-modified-p)))))
        (vemv/toggle-ns-hiding :after-file-open))
      (when t?
        (setq-local vemv/ns-shown t))
      (vemv/clean-chosen-file-buffer-order))
    (setq-local mode-line-format tabbed-line-format))

  (vemv/advice-nrepl)

  (vemv/ensure-repl-visible))

(defun vemv/after-file-open-impl (&optional skip-debouncing?)
  (interactive)
  (let* ((of-current-project? (vemv/buffer-of-current-project-or-parent? (current-buffer))))
    (vemv/after-file-open-without-project-explorer-highlighting of-current-project?)
    (when (and of-current-project?
               (not vemv/terminal-emacs?))
      (if skip-debouncing?
          (vemv/safe-show-current-file-in-project-explorer* (get-buffer-window))
        (funcall vemv/safe-show-current-file-in-project-explorer)))))

(defvar vemv.after-file-open/avoid-recursion nil)

(defun vemv.after-file-open/skipping-debouncing (&rest ignore)
  (when (vemv/buffer-of-current-project-or-parent? (current-buffer))
    (unless (eq vemv.after-file-open/avoid-recursion (current-buffer))
      (setq vemv.after-file-open/avoid-recursion (current-buffer))
      (vemv/after-file-open-impl :skip-debouncing))))

(defun vemv/after-file-open (&rest ignore)
  (interactive)
  (with-selected-window vemv/main_window
    (vemv/jump-to-first-git-modified-line)
    (vemv/after-file-open-impl)))

(defvar vemv/after-file-open-watcher
  (add-hook 'focus-in-hook 'vemv/advice-nrepl))

(defun vemv/maybe-replace-dashes-for-clj (f dir)
  (if (and (or (s-ends-with? ".clj" f)
               (s-ends-with? ".cljc" f)
               (s-ends-with? ".cljs" f))
           (s-starts-with? dir f))
      (concat dir (s-replace "-" "_" (s-replace dir "" f)))
    f))

(defun vemv/open (&optional filepath open-at-pwd)
  "Opens a file (from FILEPATH or the user input), creating it if it didn't exist already (per the provided path).
OPEN-AT-PWD decides the initial pwd of the prompt."
  (interactive)
  (vemv/safe-select-frame)
  (vemv/safe-select-window vemv/main_window)
  (let* ((default-directory (if (and (or open-at-pwd
                                         (member major-mode '(typescript-mode js-mode)))
                                     (buffer-file-name))
                                (file-name-directory (buffer-file-name))
                              (if (vemv/contains? (buffer-file-name) vemv/project-root-dir)
                                  default-directory
                                vemv/project-clojure-dir)))
         ;; magical let - do not unwrap!
         (file (buffer-name (find-file (or filepath
                                           (vemv/maybe-replace-dashes-for-clj (ido-read-file-name "(C-f to toggle completion) ")
                                                                              default-directory)))))))
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
                                                  vemv/project-clojure-dir)
                                  (not (file-directory-p the-file))))
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
  (vemv/git-file-list-for "\"^ M \\|^??\\|^UU\""))

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

;; Notes:
;; * an inline diff cannot work well, since that would easily leave unbalanced parentheses
;; * The git-gutter package is not polished enough to use.
(defun vemv/open-git-diff-against ()
  (interactive)
  (require 'magit-diff)
  (select-window vemv/main_window)
  (let* ((default-directory vemv/project-root-dir)
         (branch (magit-diff-read-range-or-commit "Branch" vemv.project/default-git-branch))
         (files (->> branch
                     (concat "git diff --name-only ")
                     (vemv/git-file-list-for nil)
                     (-filter 'file-exists-p))))


    ;; close file buffers, so that the GUI popup becomes clearer, allowing easy navigation through large diffs:
    (->> (buffer-list)
         (mapcar (lambda (b)
                   (let* ((f (buffer-file-name b)))
                     (when (and f
                                (or (s-ends-with? ".clj" f)
                                    (s-ends-with? ".cljc" f)
                                    (s-ends-with? ".cljs" f)
                                    (s-ends-with? ".edn" f)
                                    (s-ends-with? ".rb" f)
                                    (s-ends-with? ".ts" f)
                                    (s-ends-with? ".js" f)))
                       (with-current-buffer b
                         (vemv/close-this-buffer :noswitch)))))))

    (mapcar 'find-file-noselect files)
    (vemv/next-file-buffer)))
