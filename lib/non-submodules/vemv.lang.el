;; -*- lexical-binding: t; -*-

(require 'vemv.lang.core)
(provide 'vemv.lang)

(defun vemv/send (&optional where backward? content no-return)
  "Does the following, sequentially:
   * Copy the next sexp (or on truthy `backward?' arg, the previous sexp);
   * Switch to the window that is assigned for REPL purposes;
   * Switch to the corresponding REPL buffer for the current language context;
   * Paste the sexp;
   * Simulate a <RET> press;
   * Go back to the original window."
  (interactive)
  (let ((where (or where
                   (case major-mode
                     ('clojure-mode :clj)
                     ('clojurescript-mode :cljs)
                     ('clojurec-mode vemv/project-type)
                     ('emacs-lisp-mode :ielm)
                     ('inferior-emacs-lisp-mode :ielm)
                     ('sh-mode :shell)
                     ('ruby-mode :ruby))
                   :shell))
        (content (or content
                     (if (region-active-p)
                         (vemv/selected-region)
                       (vemv/sexpr-content backward?)))))
    (if (equal where :emacs)
        (eval (read content))
      (let* ((destination-buffer (case where
                                   (:ruby "*rails*")
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
          (if (and (eq where :ruby)
                   (not (get-buffer "*rails*")))
              (vemv/echo "Disconnected!")
            (with-selected-window vemv/repl-window
              (switch-to-buffer destination-buffer)

              (end-of-buffer)
              (insert content)

              (unless no-return
                (case where
                  (:ielm (ielm-return))
                  (:ruby (comint-send-input))
                  (:shell (comint-send-input))
                  (:clj (cider-repl-return))
                  (:cljs (cider-repl-return))))

              (pop kill-ring)
              (end-of-buffer))
            (when no-return
              (vemv/safe-select-window vemv/repl-window))))))))

(setq vemv/shell-id 0)

(defun vemv/sh ()
  (interactive)
  (let* ((b (get-buffer-create (concat "*shell-"
                                       (number-to-string (send! vemv/shell-id (lambda (a)
                                                                                (inc a))))
                                       "*")))
         (default-directory vemv/project-root-dir))
    (switch-to-buffer (buffer-name b) nil t)
    (shell b)
    (setq-local comint-process-echoes t) ;; disable command echoing
    ))

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

(defun vemv/keyboard-funcall (ref f &rest args)
  (if (or vemv/input-enabled (equal ref :vemv/shortcuts/global/primary-secondary-8))
      (progn
        (apply f args))
    (vemv/echo "Input disabled by `vemv/keyboard-funcall'. Press primary-secondary-8 to re-enable it.")))

(defun vemv/shell-command-to-tuple (command)
  "Like `shell-command-to-string', but returns a (<return-code> <output>) tuple instead of just <output>"
  (let* ((v nil)
         (s (with-output-to-string
              (with-current-buffer
                  standard-output
                (setq v (process-file shell-file-name nil t nil shell-command-switch command))))))
    (list v s)))
