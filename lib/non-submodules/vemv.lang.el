;; -*- lexical-binding: t; -*-

(require 'vemv.lang.core)
(provide 'vemv.lang)

;; elisp gotchas: let vs. let* · last returns a list · "Wrong type argument: commandp" -> forgot interactive

(setq vemv/apply-tests-verbosely-counter 0)

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

(defun vemv/refresh-pe-cache ()
  (vemv/safe-select-window vemv/project-explorer-window)
  (call-interactively 'pe/cache-clear)
  (funcall pe/directory-tree-function
           default-directory
           (apply-partially 'pe/set-tree (current-buffer) 'refresh))
  (vemv/safe-select-window vemv/main_window))

(setq vemv/refreshing-caches
      nil)

(setq vemv/project-explorer-initialized
      nil)

(defun vemv/timestamp-lock-acquired? (timestamp)
  (and timestamp (< (- (vemv/timestamp) timestamp) 30)))

(defun vemv/refresh-file-caches ()
  (unless (or (vemv/timestamp-lock-acquired? vemv/refreshing-caches)
              (not vemv/project-explorer-initialized)
              (minibuffer-prompt)
              (not (vemv/contains? (buffer-name (current-buffer)) ".clj")))
    (setq vemv/refreshing-caches (vemv/timestamp))
    (vemv/refresh-pe-cache)
    (fiplr-clear-cache)))

(defun vemv/open-at-project-root ()
  (interactive)
  (let ((default-directory vemv/project-root-dir))
    (call-interactively 'vemv/open)))

(defun vemv/safely-open-pe-window ()
  (when (boundp 'vemv/project-explorer-window)
    (vemv/safe-select-window vemv/project-explorer-window)))

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

(defun vemv/ensure-project-is-displayed! ()
  (vemv/save-window-excursion
   (vemv/safe-select-window vemv/project-explorer-window)
   (let* ((expected vemv/project-root-dir)
          (actual (pe/project-root-function-default))
          (default-directory expected))
     (when (not (string-equal expected actual))
       (call-interactively 'project-explorer-open)))))

(defun vemv/show-current-file-in-project-explorer-unsafe ()
  (interactive)
  (let ((fallback (argless (funcall vemv/safe-show-current-file-in-project-explorer))))
    (if (minibuffer-prompt)
        (delay fallback 1)
        
        (vemv/refresh-file-caches)
        (vemv/safe-select-window vemv/main_window)
        (if (minibuffer-prompt)
            (delay fallback 1)

            (vemv/ensure-project-is-displayed!)
            (let ((buffer-truename (file-truename (buffer-file-name))))
              (when (vemv/contains? buffer-truename vemv/project-root-dir)
                (let* ((buffer-fragments (-remove (lambda (x) (string-equal x "")) (split-string buffer-truename "/")))
                       (projname (pe/project-root-function-default)) ;; "/Users/vemv/gpm"
                       (project-fragments (-remove (lambda (x) (string-equal x "")) (split-string projname "/")))
                       (fragments (-drop (length project-fragments) buffer-fragments))
                       (expanded-fragments (mapcar* (lambda (x y)
                                                      (-take x y))
                                                    (number-sequence 1 (length fragments)) (-repeat (length fragments) fragments)))
                       (final-fragments (mapcar (lambda (x)
                                                  (concat (s-join "" (cons projname (-interpose "/" x))) "/"))
                                                expanded-fragments)))
                  
                  (vemv/safe-select-window vemv/project-explorer-window)
                  ;; (pe/fold-all) ;; necessary in principle, skip it for performance. seems to work fine.
                  (beginning-of-buffer)
                  
                  (seq-doseq (f (butlast final-fragments))
                    (while (not (string-equal f (pe/current-directory)))
                      (next-line))
                    (pe/return))
                  
                  (while (not (string-equal (s-chop-suffix "/" (first (last final-fragments))) (pe/get-filename)))
                    (next-line))
                  
                  (end-of-line))))))))

(defun vemv/safe-show-current-file-in-project-explorer* ()
  (condition-case nil
      (vemv/show-current-file-in-project-explorer-unsafe)
    (error (ignore-errors (vemv/show-current-file-in-project-explorer-unsafe))))
  (vemv/safe-select-window vemv/main_window))

(setq vemv/safe-show-current-file-in-project-explorer
      (vemv/debounce 'vemv/safe-show-current-file-in-project-explorer* 0.8))

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

(defun vemv/buffer-of-current-project? (b)
  (when (and b (buffer-file-name b))
    (vemv/contains? (file-truename (buffer-file-name b))
                    vemv/project-root-dir)))

(defun vemv/buffer-of-current-running-project? (b)
  (when (and b (buffer-file-name b))
    (vemv/contains? (file-truename (buffer-file-name b))
                    vemv/running-project-root-dir)))

(defun vemv/advice-nrepl* (&optional after)
  (interactive)
  (delay (argless
          (unless (or (vemv/scratch-p)
                      (not (vemv/buffer-of-current-running-project? (current-buffer)))
                      (and (eq vemv/running-project-type :clj) (vemv/current-main-buffer-is-cljs)))
            (when (and (vemv/ciderable-p)
                       (vemv/figwheel-connected-p)
                       (not (string-equal (vemv/current-ns)
                                          (vemv/current-ns (window-buffer vemv/repl2)))))
              (cider-repl-set-ns (vemv/current-ns))))
          (when after
            (funcall after)))
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
    (vemv/safe-select-window vemv/repl2)
    (if was
        (switch-to-buffer vemv/cljs-repl-name)
        (switch-to-buffer vemv/clj-repl-name))
    (vemv/safe-select-window vemv/main_window)))

(defun vemv/ensure-repl-visible ()
  (when (and (cider-connected-p) (string-equal cider-launched vemv/current-project))
    (vemv/show-clj-or-cljs-repl)))

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

(defun vemv.abbreviate-ns/format-intermediate-fragment (x)
  (condition-case
      nil (let* ((split (s-split "-" x))
                 (y (mapcar (lambda (f) (substring f 0 1)) split)))
            (s-join "-" y))
    (error "")))

(defun vemv/abbreviate-ns (namespace)
  (let* ((split (s-split "\\." namespace))
         (name (car (last split)))
         (bbase (-remove (lambda (x) (string-equal x vemv/project-ns-prefix)) (butlast split)))
         (fname (car bbase))
         (base (rest bbase))
         (onechars (mapcar (lambda (x)
                             (vemv.abbreviate-ns/format-intermediate-fragment x))
                           base)))
    (concat fname (if fname "." "") (s-join "." onechars) (if (> (length onechars) 0) "." "") name)))

(defun vemv/mode-line-for-buffer (buffer-name)
  (if (vemv/contains? buffer-name "project.clj")
      buffer-name
      (let* ((is-clj (vemv/contains? buffer-name ".clj"))
             (buf (get-buffer buffer-name))
             (sym (intern (concat "vemv/mode-line-for-buffer/" (buffer-file-name buf) "-open")))
             (close-sym (intern (concat "vemv/mode-line-for-buffer/" (buffer-file-name buf) "-close")))
             (namespace (if is-clj
                            (vemv/abbreviate-ns (with-current-buffer buffer-name
                                                  (or (ignore-errors
                                                        (cider-current-ns))
                                                      buffer-name)))))
             (is-modified (with-current-buffer buffer-name (buffer-modified-p)))
             (shortname (concat (if is-clj namespace buffer-name)
                                (if is-modified "*" ""))))
        (unless (fboundp sym)
          (eval `(defun ,sym ()
                   (interactive)
                   ()
                   (vemv/safe-select-window vemv/main_window)
                   (switch-to-buffer ,buffer-name)
                   (vemv/after-file-open)))
          (eval `(defun ,close-sym ()
                   (interactive)
                   (kill-buffer ,buffer-name)
                   (vemv/clean-chosen-file-buffer-order))))
        (propertize shortname 'local-map `(keymap
                                           (mode-line keymap
                                                      (mouse-1 . ,sym)
                                                      (mouse-3 . ,close-sym)))))))

(defun vemv/mode-line-for-project (project-name)
  (let* ((sym (intern (concat "vemv/mode-line-for-project/" project-name "-open")))
         (close-sym (intern (concat "vemv/mode-line-for-project/" project-name "-close"))))
    (unless (fboundp sym)
      (eval `(defun ,sym ()
               (interactive)
               (vemv/select-project-within-workspace ,project-name)
               (vemv/force-refresh-project!)))
      (eval `(defun ,close-sym ()
               (interactive)
               (when (vemv/close-project-within-workspace ,project-name)
                 (vemv/force-refresh-project!)))))
    (propertize project-name 'local-map `(keymap
                                          (mode-line keymap
                                                     (mouse-1 . ,sym)
                                                     (mouse-3 . ,close-sym))))))

(defun vemv/message-file-buffers-impl ()
  (vemv/clean-chosen-file-buffer-order)
  (let* ((x (car (gethash vemv/current-project vemv/chosen-file-buffer-order))) 
         (first (if (vemv/contains? x ".clj")
                    (if (vemv/contains? x "project.clj")
                        x
                        (vemv/abbreviate-ns (or (ignore-errors (cider-current-ns))
                                                x)))
                    x))
         (first (when x
                  (with-current-buffer (get-buffer x)
                    (if (buffer-modified-p)
                        (concat first "*")
                        first))))
         (rest (mapcar 'vemv/mode-line-for-buffer (cdr (gethash vemv/current-project vemv/chosen-file-buffer-order)))))
    (vemv/format-tabs first rest)))

(defun vemv/pe/mode-line-format* ()
  (let* ((first vemv/current-project)
         (rest (mapcar 'vemv/mode-line-for-project (cdr (vemv/all-project-names)))))
    (concat "  " (vemv/format-tabs first rest))))

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

;; not needed anymore - cider-find-var does the trick!
(defun vemv/open-namespace-at-point ()
  (let* ((ns (s-replace "." "" (vemv/copy-selection-or-next-sexpr)))
         (ns2 (s-replace "-" "" ns))
         (ns3 (concat "src/horizon/src/" ns2 ".cljs")))
    (delay (argless (insert ns3))
           2)
    (vemv/fiplr)))

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

(defun vemv/save-all-clojure-buffers-for-this-project ()
  (mapcar (lambda (b)
            (when (and (vemv/contains? (buffer-name b) ".clj")
                       (vemv/buffer-of-current-project? b))
              (vemv/save b)))
          (vemv/all-buffers)))

(setq vemv/ns-shown nil)

(defun cljr--maybe-wrap-form ()) ;; void it

;; we can use this in horizon when ns's properly use initialization patterns
(defun vemv/clean-project-namespaces ()
  (if (not vemv-cleaning-namespaces)
      (vemv/echo "vemv-cleaning-namespaces set to false!")
      (let* ((files (filter (lambda (x)
                              (vemv/ends-with x ".cljs"))
                            (directory-files-recursively "/Users/vemv/gpm/src/horizon/src/" ".cljs"))))
        (vemv/safe-select-window vemv/repl2)
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

(defun vemv/initial-layout ()
  
  (if (window-system) (vemv/maximize))

  (split-window-vertically)
  (enlarge-window 8)

  (setq default-directory vemv-home)

  (let ((default-directory vemv/project-root-dir))
    (vemv/safely-open-pe-window)
    (call-interactively 'project-explorer-open)
    (enlarge-window-horizontally -19) ;; leaves 130 columns for vemv/main_window in a 13" Macbook Air
    (setq vemv/project-explorer-window (selected-window)))

  (vemv/next-window)

  (setq vemv/main_window (selected-window))

  (vemv/next-window)

  (let ((default-directory vemv/project-root-dir))
    (sh)
    (switch-to-buffer "*scratch*"))

  (vemv/next-window)

  (setq vemv/repl2 (selected-window))

  (delay (argless (vemv/safe-select-window vemv/repl2)
                  (switch-to-buffer "*shell-1*")
                  (enable-paredit-mode)
                  (vemv/safe-select-window vemv/main_window)
                  (setq vemv/launched t))
         1)

  (vemv/next-window)
  (message ""))

(defun vemv/close-this-buffer ()
  (setq-local vemv/ns-shown nil)
  (kill-buffer (current-buffer))
  (when (and (eq (selected-window) vemv/main_window)
             (not (vemv/in-clojure-mode?)))
    (vemv/next-file-buffer))
  (when (not (member (buffer-name (current-buffer)) (gethash vemv/current-project vemv/chosen-file-buffer-order)))
    (switch-to-buffer vemv/file-buffer-fallback)))

(defun vemv/noncloseable-buffer-p ()
  (-any? (lambda (x) (vemv/contains? (buffer-name) x))
         (list vemv/clj-repl-name
               vemv/cljs-repl-name
               "project-explorer"
               "shell-1"
               "cider-repl"
               "scratch")))

(defun vemv/good-buffer-p ()
  (-any? (lambda (x) (vemv/contains? (buffer-name) x))
         (list ".clj" ".el")))

(defun vemv/good-window-p ()
  (or (eq (selected-window) vemv/main_window)
      (eq (selected-window) vemv/repl2)
      (eq (selected-window) vemv/project-explorer-window)))

(setq vemv/main_frame (selected-frame))

(defun vemv/good-frame-p ()
  (eq vemv/main_frame (selected-frame)))

(defun vemv/close-this-window ()
  (delete-window))

(defun vemv/close-this-frame ()
  (delete-frame (selected-frame) t))

(defun vemv/stop-using-minibuffer (&optional callback)
  "kill the minibuffer"
  (condition-case nil
      (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
        (when callback
          (delay callback 0.3))
        (abort-recursive-edit)
        (error nil))))

(defun vemv/close-this ()
  (interactive)
  ;; For when minibuffer gets stuck (asks for input, but minibuffer-frame is in a different buffer from the current one)
  (vemv/stop-using-minibuffer)
  (if (or (and (vemv/good-buffer-p)
               (vemv/good-window-p))
          (and (not (vemv/good-buffer-p))
               (not (vemv/noncloseable-buffer-p)))
          (and (vemv/good-buffer-p)
               (not (vemv/noncloseable-buffer-p))
               (not (vemv/good-window-p))
               (vemv/good-frame-p)))
      (vemv/close-this-buffer))
  ;; buffer closing can change the selected window. compensate it:
  (if-let (unrelated-window (first (filter (lambda (w)
                                             (not (seq-contains (list vemv/repl2 vemv/project-explorer-window vemv/main_window)
                                                                w)))
                                           (window-list))))
      (select-window unrelated-window))
  (unless (< (length (vemv/current-frame-buffers)) 2)
    (unless (vemv/good-window-p)
      (vemv/close-this-window)))
  (unless (vemv/good-frame-p)
    (vemv/close-this-frame)))

(defun vemv/close-all-file-buffers ()
  (interactive)
  (mapcar (lambda (b)
            (with-current-buffer b
              (vemv/close-this-buffer)))
          (-clone (gethash vemv/current-project vemv/chosen-file-buffer-order)))
  (switch-to-buffer "*scratch*"))

(defun vemv/close-all-other-file-buffers ()
  (interactive)
  (let ((root (buffer-name (current-buffer))))
    (mapcar (lambda (b)
              (unless (string-equal b root)
                (with-current-buffer b
                  (vemv/close-this-buffer))))
            (-clone (gethash vemv/current-project vemv/chosen-file-buffer-order)))))

(defun vemv/open-recent-file-for-this-project! ()
  (when (boundp 'vemv/main_window)
    (let* ((the-file (when (and (file-readable-p recentf-save-file)
                                (pos? (length recentf-list)))
                       (car (filter (lambda (x)
                                      (and x (vemv/contains? (file-truename x) vemv/project-root-dir)
                                           (not (vemv/contains? (file-truename x) "ido.last"))))
                                    recentf-list))))
           (the-file (if (or (not (vemv/in-clojure-mode?))
                             (and the-file
                                  (file-exists-p the-file) ;; file-truename can make up nonexisting files
                                  (vemv/contains? (file-truename the-file) ;; expand symlinks
                                                  vemv/project-clojure-dir)))
                         the-file ;; ensure nrepl opens a clojure context
                         vemv/default-clojure-file))
           (the-file (if the-file (file-truename the-file))))
      (when (and the-file (file-exists-p the-file))
        (vemv/open the-file)))))

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
                                                   (cider-interactive-eval "(prn *e)")
                                                   (delay (argless
                                                           (when (vemv/buffer-of-current-running-project?
                                                                  (vemv/save-window-excursion
                                                                   (vemv/safe-select-window vemv/main_window)
                                                                   (current-buffer)))
                                                             (vemv/save-window-excursion
                                                              (vemv/safe-select-window vemv/repl2)
                                                              (vemv/switch-to-buffer-in-any-frame vemv/clj-repl-name)
                                                              (end-of-buffer)
                                                              (paredit-backward)
                                                              (paredit-backward))))
                                                          0.7)))

      (vemv/safe-select-window vemv/main_window)
      (vemv/open-recent-file-for-this-project!)))

(defun vemv/tab ()
  (interactive)
  (or (and (or
            (vemv/in-indentation-point-p)
            (vemv/non-completable-char-p))
           (or (call-interactively 'indent-for-tab-command)
               t))
      (call-interactively 'company-complete)
      (call-interactively 'company-dabbrev)))

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
                          (if (or (eq vemv/project-type :cljs)
                                  (vemv/current-main-buffer-is-cljs)
                                  (vemv/contains? (buffer-string) "org.clojure/clojurescript"))
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

(defun vemv/clear-cider-repl-buffer (&optional no-recur)
  (interactive)
  (when (cider-connected-p)
    (vemv/save-window-excursion
     (select-window vemv/repl2)
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

(defun vemv/is-testing-ns ()
  (let ((n (cider-current-ns t)))
    (string-equal n (funcall cider-test-infer-test-ns n))))

(defun vemv/test-this-ns ()
  "Runs the tests for the current namespace if its name contains 'test', or the latest ns that did."
  (interactive)
  (when (vemv/in-clojure-mode?)
    (vemv/advice-nrepl (argless
                        (let* ((cljs (vemv/current-main-buffer-is-cljs))
                               (ns (vemv/current-ns))
                               (chosen (if (vemv/is-testing-ns)
                                           ns
                                           (if cljs
                                               vemv/latest-cljs-test-ran
                                               vemv/latest-clojure-test-ran))))
                          (when chosen
                            (setq vemv/latest-clojure-test-ran chosen)
                            (if clj
                                (call-interactively 'cider-test-run-ns-tests)
                                (vemv/send :cljs
                                           nil
                                           (concat (if (and cljs (vemv/contains? (vemv/current-ns) "smoke"))
                                                       "(.reload js/location true) "
                                                       "")
                                                   "(cljs.test/run-tests '"
                                                   chosen
                                                   ")")))))))))

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


