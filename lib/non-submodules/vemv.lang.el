;; -*- lexical-binding: t; -*-

(require 'vemv.lang.core)
(provide 'vemv.lang)

;; elisp gotchas: let vs. let* · last returns a list · "Wrong type argument: commandp" -> forgot interactive

(defun vemv/send (&optional where backward? content no-return)
  "Copy the next sexp (or on non-nil backward? arg, the previous sexp) and its character trailer,
  switch to the window that is assigned for REPL purposes, then it switch to the corresponding buffer
  (different REPLs have different buffers),
  paste and simulate a <RET> press. Finally, go back to sender window."
  (interactive)
  (let ((where (or where
                   (case major-mode
                     ('clojure-mode :clj)
                     ('clojurescript-mode :cljs)
                     ('clojurec-mode vemv/project-type)
                     ('emacs-lisp-mode :ielm)
                     ('inferior-emacs-lisp-mode :ielm)
                     ('sh-mode :shell))))
        (content (or content
                     (if (region-active-p)
                         (vemv/selected-region)
                       (vemv/sexpr-content backward?)))))
    (if (equal where :emacs)
        (eval (read content))
      (let* ((sender (selected-window))
             (destination-buffer (case where
                                   (:ielm "*ielm*")
                                   (:shell "*shell-1*")
                                   (:clj vemv/clj-repl-name)
                                   (:cljs vemv/cljs-repl-name)))
             (foreign? (not (seq-contains (vemv/all-buffer-names) destination-buffer)))
             (destination-buffer (if foreign?
                                     (buffer-name (window-buffer vemv/repl-window))
                                   destination-buffer)))
        (if (and (vemv/in-a-clojure-mode?)
                 foreign?
                 (not vemv/parent-project-root-dirs)) ;; implementation could be more accurate, does the job for now
            (vemv/echo "Can't eval in a different project!")
          (vemv/safe-select-window vemv/repl-window)
          (switch-to-buffer destination-buffer)

          (end-of-buffer)
          (insert content)

          (unless no-return
            (case where
              (:ielm (ielm-return))
              (:shell (comint-send-input))
              (:clj (cider-repl-return))
              (:cljs (cider-repl-return))))

          (pop kill-ring)
          (end-of-buffer))
        (unless no-return
          (vemv/safe-select-window sender))))))

(setq vemv/shell-id 0)

(defun vemv/sh ()
  (interactive)
  (let* ((b (get-buffer-create (concat "*shell-"
                                       (number-to-string (send! vemv/shell-id (lambda (a)
                                                                                (inc a))))
                                       "*")))
         (default-directory vemv/project-root-dir))
    (switch-to-buffer (buffer-name b) nil t)
    (shell b)))

(defun vemv/smex ()
  (when vemv/launched
    (smex)))

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
