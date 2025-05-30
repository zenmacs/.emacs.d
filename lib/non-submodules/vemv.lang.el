;; -*- lexical-binding: t; -*-

(require 'vemv.lang.core)
(provide 'vemv.lang)

(defun vemv/safe-switch-to-buffer (b &rest args)
  (apply 'switch-to-buffer
         (or (and b
                  (get-buffer b))
             (user-error (concat "Could not find buffer: " (prin1-to-string b))))
         args))

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
                     ('clojurec-mode :clj)
                     ('cider-repl-mode :clj)
                     ('cider-test-report-mode :clj)
                     ('cider-stacktrace-mode :clj)
                     ('cider-inspector-mode :clj)
                     ('clojurescript-mode :cljs)
                     ('clojurec-mode vemv/project-type)
                     ('emacs-lisp-mode :ielm)
                     ('inferior-emacs-lisp-mode :ielm)
                     ('sh-mode :shell)
                     ('ruby-mode :ruby)
                     ('typescript-mode :typescript))
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
                                   (:clj (or vemv/clj-repl-name
                                             (when-let* ((b (cider-current-repl 'clj nil)))
                                               (buffer-name b))))
                                   (:cljs (or vemv/cljs-repl-name
                                              (when-let* ((b (cider-current-repl 'cljs nil)))
                                                (buffer-name b))))))
             (foreign? (not (seq-contains (vemv/all-buffer-names) destination-buffer)))
             (destination-buffer (if foreign?
                                     (buffer-name (window-buffer vemv/repl-window))
                                   destination-buffer))
             (destination-buffer (if (equal where :clj)
                                     (vemv/safe-clj-repl-name destination-buffer)
                                   destination-buffer)))
        ;; I don't remember why I implemented "Can't eval in a different project!" rule in the first place.
        ;; Probably related with a cljs-specific pain point.
        (if (and (with-current-buffer (window-buffer vemv/main_window)
                   (vemv/current-buffer-is-cljs))
                 foreign?
                 (not vemv/parent-project-root-dirs)) ;; implementation could be more accurate, does the job for now
            (vemv/echo "Can't eval in a different project!")
          (if (and (eq where :ruby)
                   (not (get-buffer "*rails*")))
              (vemv/echo "Disconnected!")
            (with-selected-window vemv/repl-window
              (vemv/safe-switch-to-buffer destination-buffer)

              (end-of-buffer)
              (insert content)

              (when (member where (list :clj :cljs))
                (end-of-buffer)
                (paredit-backward)
                (ignore-errors ;; don't choke at "text is read-only". happens when sending a sexpr with leading whitespace.
                  (when no-return
                    (paredit-wrap-round)
                    (insert "-> "))
                  (vemv/indent))
                (unless no-return
                  (end-of-buffer)))

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
    (vemv/safe-switch-to-buffer (buffer-name b) nil t)
    (shell b)
    (setq-local comint-process-echoes t) ;; disable command echoing
    ))

(defun vemv/smex ()
  (if vemv/launched
      (smex)
    (error "`vemv/launched` is nil. Check *Messages*")))

(defun vemv/emacs-reload ()
  (let ((was-verbose vemv/verbose-mode))
    (when (not was-verbose)
      (vemv/toggle-verbosity))
    ;; XXX more files? (follow a logical order)
    (load "vemv.lang")
    (load "vemv.project")
    (load "vemv.workspace")
    (load "vemv.data.bindings")
    (load "vemv.shortcuts.global")
    (load "vemv.shortcuts.clojure")
    (load "vemv.shortcuts.ruby")
    (load "vemv-theme")
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

(defun vemv/unescape (s)
  "Removes escaped escapings from the string `s'."
  (read (format "\"%s\"" s)))
