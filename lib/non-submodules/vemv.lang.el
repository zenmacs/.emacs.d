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

(setq vemv/shell-id 0)

(defun sh ()
  (interactive)
  (shell (concat "*shell-" (number-to-string (send! vemv/shell-id (lambda (a) (inc a)))) "*")))

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
