;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.clojure-interaction)

(setq vemv/apply-tests-verbosely-counter 0)

(defun vemv/ciderable-p ()
  (and
   (vemv/in-clojure-mode?)
   (cider-connected-p)
   vemv-cider-connected))

(defun vemv/cider-formattable-p ()
  (and (vemv/ciderable-p)
       (not (eq clojure-indent-style :align-arguments))
       (not clojure-align-forms-automatically)))

(defun vemv/apply-tests-verbosely (f &rest args)
  (let* ((old vemv/verbose-mode)
         (counter vemv/apply-tests-verbosely-counter)
         (setter (lambda (&rest _)
                   (when (eq counter vemv/apply-tests-verbosely-counter)
                     (vemv/set-verbosity-to old)
                     (setq vemv/apply-tests-verbosely-counter (+ 1 vemv/apply-tests-verbosely-counter))))))
    (vemv/set-verbosity-to t)
    (advice-add 'cider-test-echo-summary :after setter)
    (apply f args)))

(defun vemv/current-ns (&optional which-buffer)
  (with-current-buffer (buffer-name which-buffer)
    (cider-current-ns)))

(setq vemv/figwheel-connected-p-already nil)

;; XXX this should be a universal figwheel fn. open PR at some point
(defun vemv/figwheel-connected-p ()
  (if (or
       vemv/figwheel-connected-p-already
       (not (vemv/current-main-buffer-is-cljs))
       (not (string-equal vemv/current-project "gpm")))
      t
      (condition-case nil
          (with-current-buffer vemv/clj-repl-name
            (progn (cider-nrepl-sync-request:eval "(require 'dev.formatting.watch)")
                   (if (string-equal (nrepl-dict-get (cider-nrepl-sync-request:eval "(dev.formatting.watch/currently-connected?)")
                                                     "value")
                                     "true")
                       (progn
                         (setq vemv/figwheel-connected-p-already t)
                         t)
                       nil)))
        (error nil))))


(defun vemv/advice-nrepl* (&optional after)
  (interactive)
  (delay (argless
          (unless (or (vemv/scratch-p)
                      (not (vemv/buffer-of-current-running-project-or-children? (current-buffer)))
                      (and (eq vemv/running-project-type :clj) (vemv/current-main-buffer-is-cljs)))
            (when (and (vemv/ciderable-p)
                       (vemv/figwheel-connected-p)
                       (not (string-equal (vemv/current-ns)
                                          (vemv/current-ns (window-buffer vemv/repl-window)))))
              (cider-repl-set-ns (vemv/current-ns))
              (cider-interactive-eval (concat "(try (clojure.core/require '"
                                              (vemv/current-ns)
                                              ") (catch Throwable _))")))
            (when after
              (funcall after))))
         1))

(setq vemv/debounced-advice-nrepl (vemv/debounce 'vemv/advice-nrepl* 0.8))

(defun vemv/advice-nrepl (&optional x)
  (funcall vemv/debounced-advice-nrepl x))

(defun vemv/toggle-ns-hiding (&optional after-file-open)
  (interactive)
  (when (not vemv-cleaning-namespaces)
    (let ((curr-buff-name (buffer-name (current-buffer))))
      (setq-local vemv/ns-shown (if after-file-open
                                    (if vemv/ns-shown
                                        vemv/ns-shown
                                        nil)
                                    (if vemv/ns-shown
                                        nil
                                        curr-buff-name)))
      (if vemv/ns-shown
          (hs-show-all)
          (let* ((hs-block-start-regexp "(ns")
                 (hs-block-end-regexp ")")
                 (hs-hide-comments-when-hiding-all nil)
                 (hs-adjust-block-beginning (lambda (initial)
                                              (save-excursion
                                                (point)))))
            (apply #'hs-hide-all ()))))))

(defun vemv/show-clj-or-cljs-repl ()
  (when (vemv/ciderable-p)
    (vemv/safe-select-window vemv/main_window)
    (setq was (vemv/current-main-buffer-is-cljs))
    (vemv/safe-select-window vemv/repl-window)
    (if was
        (switch-to-buffer vemv/cljs-repl-name)
        (switch-to-buffer vemv/clj-repl-name))
    (vemv/safe-select-window vemv/main_window)))

(defun vemv/ensure-repl-visible ()
  (when (and (cider-connected-p) (string-equal cider-launched vemv/current-project))
    (vemv/show-clj-or-cljs-repl)))

(setq vemv/ns-shown nil)

(defun cljr--maybe-wrap-form ()) ;; void it

;; we can use this in horizon when ns's properly use initialization patterns
(defun vemv/clean-project-namespaces ()
  (if (not vemv-cleaning-namespaces)
      (vemv/echo "vemv-cleaning-namespaces set to false!")
      (let* ((files (filter (lambda (x)
                              (vemv/ends-with x ".cljs"))
                            (directory-files-recursively "/Users/vemv/gpm/src/horizon/src/" ".cljs"))))
        (vemv/safe-select-window vemv/repl-window)
        (switch-to-buffer "*Messages*")
        (vemv/safe-select-window vemv/main_window)
        (vemv/open "/Users/vemv/gpm/src/horizon/project.clj")
        (seq-doseq (filename files)
          (vemv/safe-select-window vemv/main_window)
          (vemv/open filename)
          (setq lexical-binding t)
          (setq whitespace-line-column 240)
          (cljr-clean-ns)
          (beginning-of-buffer)
          (while (re-search-forward "(:require[^\-]" nil t)
            (replace-match "(:require\n"))
          (beginning-of-buffer)
          (while (re-search-forward "(:require\-macros[^\n]" nil t)
            (replace-match "(:require\-macros\n"))
          (beginning-of-buffer)
          (while (re-search-forward "(:import[^\-]" nil t)
            (replace-match "(:import\n"))
          (beginning-of-buffer)
          (while (re-search-forward "(:use\-macros[^\n]" nil t)
            (replace-match "(:use\-macros\n"))
          (vemv/save)
          (vemv/save)
          (vemv/close-this-buffer)))
      (vemv/echo "clean-project-namespaces done!")
      (vemv/echo "Remember: goog* libspec can be spuriously removed.")))

(setq vemv/cljr-building-ast-cache? nil) ;; XXX implement on new clj-refactor.el release

(defun vemv/load-clojure-buffer ()
  (interactive)
  (when (vemv/ciderable-p)
    (if (vemv/current-main-buffer-is-cljs)
        (vemv/send :cljs nil "(.reload js/location true)")
        (progn
          (vemv/save)
          (vemv/save) ;; save autoformatting
          (vemv/advice-nrepl)
          (replying-yes
           (if vemv/using-component-reloaded-workflow
               (if vemv/cljr-building-ast-cache?
                   (message "Currently building AST cache. Wait a few seconds and try again.")
                   (progn
                     (cider-interactive-eval (or vemv/clojure-reload-command
                                                 "(with-out-str (com.stuartsierra.component.user-helpers/reset))"))
                     (delay (argless (message "Reloaded!"))
                            0.1)))
               (progn
                 (cider-load-buffer)
                 (cider-load-all-project-ns)
                 (delay (argless (message "Reloaded!"))
                        0.1))))))))

(defun vemv/clojure-init ()
  (if (minibuffer-prompt)
      (delay 'vemv/clojure-init 1)

      (advice-add 'pe/show-buffer :after 'vemv/after-file-open)
      (advice-add 'vemv/fiplr :after 'vemv/after-file-open)
      ;; (advice-add 'vemv/open :after 'vemv/after-file-open) ;; I don't remember why I disabled this
      (advice-add 'vemv/next-file-buffer :after 'vemv/after-file-open)
      (advice-add 'vemv/previous-file-buffer :after 'vemv/after-file-open)
      (advice-add 'vemv/close-this-buffer :after 'vemv/after-file-open)
      (advice-add 'helm-ag--action-find-file :after 'vemv/after-file-open)
      (advice-add 'cider-new-error-buffer :after (lambda (&rest _)
                                                   (cider-interactive-eval "(try (clojure.core/prn clojure.core/*e)
                                                                              (catch Throwable e))")
                                                   (delay (argless
                                                           (when (vemv/buffer-of-current-running-project?
                                                                  (vemv/save-window-excursion
                                                                   (vemv/safe-select-window vemv/main_window)
                                                                   (current-buffer)))
                                                             (vemv/save-window-excursion
                                                              (vemv/safe-select-window vemv/repl-window)
                                                              (vemv/switch-to-buffer-in-any-frame vemv/clj-repl-name)
                                                              (end-of-buffer)
                                                              (paredit-backward)
                                                              (paredit-backward))))
                                                          0.7)))

      (vemv/safe-select-window vemv/main_window)
      (vemv/open-recent-file-for-this-project!)))

(defun vemv/is-cljs-project? ()
  (or (eq vemv/project-type :cljs)
      (vemv/current-main-buffer-is-cljs)
      (if-let ((p (vemv/project-dot-clj-file)))
          (let* ((was-open (get-file-buffer p))
                 (_ (unless was-open
                      (find-file-noselect p)))
                 (ret (with-current-buffer (get-file-buffer p)
                        (vemv/contains? (buffer-string) "org.clojure/clojurescript"))))
            (unless was-open
              (kill-buffer (get-file-buffer p)))
            ret))))

(defun vemv/clojure-init-or-send-sexpr ()
  (interactive)
  (when (vemv/in-clojure-mode?)
    (if (and (not cider-launched) vemv/using-nrepl)
        (progn
          (setq cider-launched vemv/current-project)
          (setq vemv-cider-connecting t)
          (setq vemv/running-project vemv/current-project)
          (setq vemv/running-project-root-dir vemv/project-root-dir)
          (setq vemv/running-project-type vemv/project-type)
          (delay (argless (funcall vemv/project-initializers)
                          (select-window vemv/main_window)
                          (if (vemv/is-cljs-project?)
                              (cider-jack-in-clojurescript)
                              (if vemv/cider-port
                                  (cider-connect "127.0.0.1" vemv/cider-port vemv/project-root-dir)
                                  (cider-jack-in))))
                 1))
        (if vemv/using-nrepl
            (if (cider-connected-p)
                (if (vemv/current-main-buffer-is-cljs)
                    (vemv/send :cljs)
                    (vemv/send :clj)))
            (vemv/send :shell)))))

(setq vemv/cider-prompt-for-symbol cider-prompt-for-symbol)

(defun vemv/jump-to-clojure-definition ()
  (interactive)
  (if (not (vemv/in-clojure-mode?))
      (call-interactively 'xref-find-definitions)
      (let* ((curr-token (cider-symbol-at-point 'look-back))
             (curr-token-is-qualified-kw (vemv/starts-with curr-token "::")))
        (setq cider-prompt-for-symbol nil)
        (if curr-token-is-qualified-kw
            (call-interactively 'cider-find-keyword)
            (cider-find-var))
        (setq cider-prompt-for-symbol vemv/cider-prompt-for-symbol)
        (vemv/advice-nrepl))))

(defun vemv/clear-cider-repl-buffer (&optional no-recur)
  (interactive)
  (when (cider-connected-p)
    (vemv/save-window-excursion
     (select-window vemv/repl-window)
     (end-of-buffer)
     (when (and (not no-recur)
                (or (> (point-max) 5000) ;; b/c I think the code below is slow, can hang emacs
                    (vemv/contains? (prin1-to-string (buffer-string))
                                    "cider-repl-stdout-face")))
       (cider-repl-return) ;; ub-hijack the prompt
       (cider-repl-clear-buffer)
       (delay (argless (vemv/clear-cider-repl-buffer :no-recur)) 1.5))
     (cider-repl-clear-buffer)
     (end-of-buffer))))

(setq vemv/latest-clojure-test-ran nil)
(setq vemv/latest-cljs-test-ran nil)

; XXX better impl: is ns inside :source-paths?
(defun vemv/is-testing-ns (&optional n inferred)
  (let* ((n (or n (cider-current-ns t))))
    (or (string-equal n (or inferred (funcall cider-test-infer-test-ns n)))
        (vemv/starts-with n "unit."))))

(defun vemv/test-this-ns ()
  "Runs the tests for the current namespace, or if not applicable, for the latest applicable ns."
  (interactive)
  (when (vemv/in-clojure-mode?)
    (vemv/advice-nrepl (argless
                        (cider-load-buffer nil
                                           (argless
                                            (let* ((cljs (vemv/current-main-buffer-is-cljs))
                                                   (ns (vemv/current-ns))
                                                   (inferred (funcall cider-test-infer-test-ns ns))
                                                   (chosen (if (vemv/is-testing-ns ns inferred)
                                                               ns
                                                               (if cljs
                                                                   vemv/latest-cljs-test-ran
                                                                   vemv/latest-clojure-test-ran))))
                                              (when chosen
                                                (setq vemv/latest-clojure-test-ran chosen)
                                                (if cljs
                                                    (vemv/send :cljs
                                                               nil
                                                               (concat "(cljs.test/run-tests '"
                                                                       chosen
                                                                       ")"))
                                                    (vemv/echo chosen)
                                                    (cider-test-execute chosen nil nil))))))))))

(defun vemv/run-this-deftest ()
  "Assuming `point` is at a deftest name, it runs it"
  (interactive)
  (vemv/advice-nrepl (argless
                      (let* ((cljs (vemv/current-main-buffer-is-cljs))
                             (ns (vemv/current-ns))
                             (chosen (cider-symbol-at-point)))
                        (when chosen
                          (vemv/send (if cljs :cljs :clj)
                                     nil
                                     (concat (if cljs "(.reload js/location true) " "")
                                             "(cljs.test/run-block ["
                                             chosen
                                             "])")))))))

(defun vemv/dumb-cljs-test ()
  "Needs M-x cider-load-buffer first"
  (interactive)
  (vemv/send :cljs nil (concat "(cljs.test/run-tests '" (vemv/current-ns) ")")))
