;; -*- lexical-binding: t; -*-

(require 'recur)
(require 'multi-methods)
(provide 'vemv.lang)

;; elisp gotchas: let vs. let* · last returns a list · "Wrong type argument: commandp" -> forgot interactive

(defmacro comm (&rest forms)
  "Comment. Doesn't evaluate its arguments, returns nil."
  nil)

(defmacro argless (&rest forms)
  "Shortcut for (lambda () (interactive) ,@forms)"
  `(lambda () (interactive) ,@forms))

(defmacro if-let (binding &rest forms)
  "Usage: (if-let (x (some-computation))
                (then x)
                (else x) (else_2) ... (else_N))"
  (let ((symbol (first binding))
        (value (second binding)))
    `(let ((,symbol ,value))
       (if ,symbol
            ,@forms))))

(defun vemv/echo (&rest xs)
  (let ((what (apply 'concat xs)))
    (setq inhibit-message nil)
    (message what)
    (setq inhibit-message t)
    what))

(defun delay (f &optional seconds)
  "Calls f in one or SECONDS seconds."
  (run-at-time (concat (int-to-string (or seconds 1)) " seconds") nil f))

(defmacro conj! (seq item) ;; Functionality doesn't require a macro - setq does as it is a special form.
  `(setq ,seq (cons ,item ,seq)))

(defmacro send! (x f &rest args) ;; Same here: functionality doesn't require a macro - setq does as it is a special form.
  "Sets x to the result of (f x args)"
  `(setq ,x (apply ,f (cons ,x (list ,@args)))))

(defmacro pos? (x)
  "(> x 0)"
  `(> ,x 0))

(defmacro neg? (x)
  "(< x 0)"
  `(< ,x 0))

(defmacro inc (n)
  `(+ ,n 1))

(defmacro dec (n)
  `(- ,n 1))

(recur-defun* vemv/take (n seq &optional acc)
              ""
              (if (and seq (pos? n))
                (recur (dec n) (rest seq) (cons (first seq) acc))
                (when (zero? n)
                  (reverse acc))))

(recur-defun* vemv/drop (n seq)
              ""
              (if (pos? n)
                (recur (dec n) (rest seq))
                seq))

(recur-defun*
 vemv/partition
 (n seq &optional step acc)
 "Divides SEQ in a list of lists of N items each, at offsets STEP or N apart. ACC is an implementation detail - do not pass this parameter!"
 (if seq
   (recur n ;; XXX recur takes the args in mistaken order. wut
          (vemv/drop (or step n) seq)
          (if-let (taken (vemv/take n seq))
            (cons taken acc)
            acc)
          (or step n))
   (reverse acc)))

(defun vemv/debounce (func &optional delay)
  (let*
      ((callee
        (cond
         ((and
           (not (symbolp func)) (functionp func)) `',func)
         ((boundp func) (symbol-value func))
         (t `',func)))
       (delay (if (not delay) 0.100 delay))
       (timer (intern (concat "timer-" (symbol-name func)))))
      
    (progn
      (set timer nil)
      `(lambda
         (&rest args)
         (progn
           (if
               (and (vectorp ,timer) (not (aref ,timer 0)))
               (cancel-timer ,timer))
           (setq
            ,timer
            (run-at-time
             ,delay nil
             (lambda
               (params)
               (apply ,callee params))
             args)))))))

(defun vemv/contains? (a b)
  "Whether the string B is contained in A."
  (let* ((a-list (string-to-list a))
         (b-list (string-to-list b))
         (a-parted (vemv/partition (length b-list) a-list 1)))
        (some (lambda (slice)
                      (equal slice b-list))
              a-parted)))

(defun vemv/maximize ()
  "Maximize the current frame. Presumes an X-window environment."
  (toggle-frame-maximized))

(defun vemv/hash-map (&rest kvs)
  "Makes and returns a hash table out of its arguments."
  (let ((result (make-hash-table :test 'equal)))
    (dolist (kv (vemv/partition 2 kvs))
            (puthash (first kv) (second kv) result))
    result))

(defun vemv/mutate-list-to (from to)
  (setcar from (car to))
  (setcdr from (rest to))
  from)

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

(defun vemv/selected-region ()
  "Returns the selected region as a string. Side effects free."
  (kill-ring-save (mark) (point))
  (let ((result (substring-no-properties (car kill-ring))))
    (pop kill-ring)
    result))

(defun vemv/current-frame-buffers ()
  (mapcar #'buffer-name (mapcar #'window-buffer (window-list))))

(defun vemv/all-buffers ()
  (buffer-list))

(defun vemv/all-buffer-names ()
  (mapcar #'buffer-name (vemv/all-buffers)))

(defun vemv/switch-to-buffer-in-any-frame (buffer-name)
  (if (seq-contains (vemv/current-frame-buffers) buffer-name)
    (switch-to-buffer buffer-name)
    (switch-to-buffer-other-frame buffer-name)))

(defun vemv/sexpr-content (&optional backward?)
  "Returns the content of the next (or previous, on non-nil values of BACKWARD?) sexpr, as a string.

Unlike paredit-copy-as-kill, this function will only grab one sexpr (and no more even - if they are contigous), and is side-effect free."
  (interactive)
  (save-excursion
   (push-mark)
   (if backward? (paredit-backward) (paredit-forward))

   (let ((result (vemv/selected-region)))
     (pop-mark)
     (if backward? (paredit-forward) (paredit-backward))
     result)))

(defun vemv/safe-select-window (x)
  (unless (minibuffer-prompt)
    (select-window x)))

(defun vemv/send (where &optional backward? content)
  "Copy the next sexp (or on non-nil backward? arg, the previous sexp) and its character trailer,
switch to the window that is assigned for REPL purposes, then it switch to the corresponding buffer (different REPLs have different buffers),
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
          (vemv/switch-to-buffer-in-any-frame destination-buffer)

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

(defun vemv/duplicate (&optional backward?)
  "Copies the current line (or sexpr, if point is at the beggining of one, or selection, if the region is active), inserting it at a new line."
  (interactive)

  (if (region-active-p)

    (progn
     (dotimes (i (- (region-end) (point)))
       (forward-char))
     (insert "\n" (vemv/selected-region) "\n"))
     
    (back-to-indentation)
     
    (if (some (lambda (char) (equal char (vemv/current-char-at-point)))
              '("(" "[" "{" "<" "\""))
      (progn
       (let ((content (vemv/sexpr-content))
             (at-b (vemv/at-beginning-of-line-p)))
         (paredit-forward)
         (insert (concat (if at-b "\n\n" "\n") content))
         (paredit-backward)
         (beginning-of-line)
         (indent-for-tab-command)))

      (progn
       (move-beginning-of-line 1)
       (kill-line)
       (yank)
       (open-line 1)
       (next-line 1)
       (yank)
       (pop kill-ring)))))

(defun vemv/kill (&optional backward? skip-save-to-clipboard?) ;; XXX kill comments FIXME can leave sexprs unmatched
  "Deletes the next (or previous, on non-nil values of BACKWARD?) sexpr or comment (if there is one).

Unlike paredit-kill, this function will only grab one sexpr (and no more, if they are contigous), and it doesn't alter the kill-ring."
  (interactive)
  (ignore-errors
   (push-mark)
   (if backward? (paredit-backward) (paredit-forward))

   (let ((result (vemv/selected-region)))
     (delete-region (mark) (point))
     (while (and
             (equal " " (vemv/current-char-at-point))
             (not (equal "\n" (vemv/current-char-at-point))))
       (paredit-forward-delete))
     (when (not skip-save-to-clipboard?)
       (simpleclip-set-contents result))
     result)))

(defun vemv/delete-backward (&optional cut?)
  "Performs a paredit-backward-delete unless the region is active, in which case the selection gets unconditionally removed.

The removed value will be pushed to the kill-ring only on non-nil values of CUT?.

Unconditionally removing code may yield semantically wrong results, i.e. leaving sexprs unmatched. I personally like this tradeoff - use with caution!"
  (interactive)

  (funcall (if cut?
              'vemv/bounded-list/insert-at-head!
              'vemv/bounded-list/insert-at-second-position!)
            (if (region-active-p)
              (progn (call-interactively 'kill-region)
                     (if (not cut?) (pop kill-ring)))
              
              (paredit-backward-delete))
            vemv/kill-list
            vemv/kill-list-bound))

(defun vemv/active-modes ()
  "Returns a list of the minor modes that are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                                         (if (and (symbolp mode) (symbol-value mode))
                                           (add-to-list 'active-modes mode))
                                         (error nil)))
          minor-mode-list)
    active-modes))

(defun vemv/next-window ()
  "Switch to the next window."
  (interactive)
  (unless (minibuffer-prompt)
    (vemv/safe-select-window (next-window))))

(defun vemv/previous-window ()
  "Switch to the previous window."
  (interactive)
  (unless (minibuffer-prompt)
    (vemv/safe-select-window (previous-window))))

(defun vemv/elisp-window-documentation ()
  "Displays the documentation for the symbol that is currently hovered by the point in a new window. Presumes emacs-lisp-mode."
  (interactive)
  (if-let (f (function-called-at-point))
    (describe-function f)))

(defun vemv/reverse (seq)
  (typecase seq
            (string (concat (reverse (string-to-list seq))))))

(defun vemv/ends-with (s ending)
  "Returns non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(defun vemv/starts-with (s candidate)
  "Returns non-nil if string S starts with CANDIDATE."
  (let ((clength (length candidate)))
    (if (<= clength (length s))
      (string= (substring s 0 clength) candidate))))

(defun vemv/keyword-to-string (arg)
  ":foo -> \"foo\""
  (substring (symbol-name arg) 1))

(defun vemv/current-line-contents ()
  "Returns the content of the line at which the point is currently located. Side effects free."
  (interactive)
  (let ((result (buffer-substring-no-properties (line-beginning-position 1) (line-beginning-position 2))))
    (if (equal result "") ;; abstact away EOFs
      "\n"
      result)))

(defun vemv/current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun vemv/current-line-number ()
  (1+ (count-lines 1 (point))))

(defun vemv/current-char-at-point (&optional offset)
  "Returns the character -as a string- hovered by the point, or a contiguous one, if an integer offset is specified."
  (interactive)
  (kill-ring-save (+ 1 (point) (or offset 0)) (+ (point) (or offset 0)))
  (let ((result (substring-no-properties (car kill-ring))))
    (pop kill-ring)
    result))

(defun vemv/indent ()
  "Indents the next sexpr."
  (interactive)
  (push-mark)
  (paredit-forward)
  (call-interactively 'cider-format-region)
  (pop-mark)
  (paredit-backward))

(defun vemv/timestamp ()
  (truncate (float-time)))

(defun vemv/refresh-pe-cache ()
  (vemv/safe-select-window vemv/project-explorer-window)
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

(defun vemv/open (&optional filepath)
  "Opens a file (from FILEPATH or the user input)."
  (interactive)
  (vemv/safe-select-window vemv/main_window)
  (let ((file (buffer-name (or (and filepath (find-file filepath)) (ido-find-file)))))) ;; magical let - do not unwrap!
  (save-buffer)
  (vemv/refresh-file-caches)
  (vemv/safe-select-window vemv/main_window))

(defun vemv/open-project ()
  (let ((default-directory (replace-regexp-in-string "\\.$" "" (ido-read-file-name ()))))
    (call-interactively 'project-explorer-open)))

(defun vemv/show-current-file-in-project-explorer-unsafe ()
  (interactive)
  (let ((fallback (argless (funcall vemv/safe-show-current-file-in-project-explorer))))
    (if (minibuffer-prompt)
        (delay fallback 1)
        
        (vemv/refresh-file-caches)
        (vemv/safe-select-window vemv/main_window)
        (if (minibuffer-prompt)
            (delay fallback 1)
            
            (let ((buffer-truename (file-truename (buffer-file-name))))
              (when (vemv/contains? buffer-truename vemv/project-root-dir)
                (let* ((buffer-fragments (-remove (lambda (x) (string-equal x "")) (split-string buffer-truename "/")))
                       (projname (pe/project-root-function-default)) ;; "/Users/vemv/gpm"
                       (project-fragments (-remove (lambda (x) (string-equal x "")) (split-string projname "/")))
                       (fragments (-drop (length project-fragments) buffer-fragments))
                       (expanded-fragments (mapcar* (lambda (x y) (-take x y)) (number-sequence 1 (length fragments)) (-repeat (length fragments) fragments)))
                       (final-fragments (mapcar (lambda (x) (concat (s-join "" (cons projname (-interpose "/" x))) "/")) expanded-fragments)))
                  
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

(defun vemv/current-ns (&optional which-buffer)
  (with-current-buffer (buffer-name which-buffer)
    (cider-current-ns)))

(setq vemv/figwheel-connected-p-already nil)

;; XXX this should be a universal fighwheel fn. open PR at some point
(defun vemv/figwheel-connected-p ()
  (if (or
        vemv/figwheel-connected-p-already
        (not (vemv/contains? (buffer-name) ".cljs"))
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
  (vemv/contains? (file-truename (buffer-file-name b))
                  vemv/running-project-root-dir))

(defun vemv/ciderable-p ()
  (vemv/contains? (buffer-name) ".clj")
  (cider-connected-p)
  vemv-cider-connected)

(defun vemv/advice-nrepl* (&optional after)
  (interactive)
  (delay (argless
          (unless (or (vemv/scratch-p)
                      (not (vemv/buffer-of-current-project? (current-buffer)))
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
  (vemv/safe-select-window vemv/main_window)
  (setq was (vemv/current-main-buffer-is-cljs))
  (vemv/safe-select-window vemv/repl2)
  (if was
    (switch-to-buffer vemv/cljs-repl-name)
    (switch-to-buffer vemv/clj-repl-name))
  (vemv/safe-select-window vemv/main_window))

(defun vemv/ensure-repl-visible ()
  (when (and (cider-connected-p) (string-equal cider-launched vemv/current-project))
    (vemv/show-clj-or-cljs-repl)))

(defun vemv/scratch-p ()
  (string-equal "*scratch*" (buffer-name (current-buffer))))

(defun vemv/after-file-open (&rest ignore)
  (interactive)
  (vemv/safe-select-window vemv/main_window)
  (when (and (vemv/contains? (buffer-name) ".clj")
             (not vemv/ns-shown))
    (vemv/toggle-ns-hiding :after-file-open))
  
  (vemv/advice-nrepl)
  (vemv/ensure-repl-visible)
  (funcall vemv/safe-show-current-file-in-project-explorer))

(defun vemv/open_file_buffers ()
  (let ((c (mapcar (lambda (x) (buffer-name x)) (buffer-list))))
    (filter (lambda (filename) (vemv/contains? filename ".clj")) c)))

(setq vemv/chosen-file-buffer-order nil) ;; a list

(defun vemv/clean-chosen-file-buffer-order ()
  "Removes closed buffers from vemv/chosen-file-buffer-order"
  (let* ((curr (buffer-name (current-buffer)))
         (actually-open (vemv/open_file_buffers))
         (all (-distinct (-concat vemv/chosen-file-buffer-order actually-open)))
         (all-without-curr (-remove (lambda (x) (string-equal x curr)) all))
         (final (cons curr all-without-curr)))
        (setq vemv/chosen-file-buffer-order (filter (lambda (x)
                                                            (member x actually-open))
                                                    final))))

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

(defun vemv/message-file-buffers-impl ()
  (vemv/clean-chosen-file-buffer-order)
  (let* ((first (vemv/abbreviate-ns (or (ignore-errors (cider-current-ns)) (car vemv/chosen-file-buffer-order))))
         (rest (cdr vemv/chosen-file-buffer-order))
         (the-rest (mapcar (lambda (x)
                                   (let* ((buf (get-buffer x))
                                          (sym (intern (buffer-file-name buf)))
                                          (close-sym (intern (concat (buffer-file-name buf) "-close")))
                                          (namespace (with-current-buffer x (or (ignore-errors (cider-current-ns)) x)))
                                          (is-modified (with-current-buffer x (buffer-modified-p)))
                                          (shortname (concat (if is-modified "*" "") (vemv/abbreviate-ns namespace))))
                                         (eval `(defun ,sym ()
                                                  (interactive)
                                                  ()
                                                  (vemv/safe-select-window vemv/main_window)
                                                  (switch-to-buffer ,x)
                                                  (vemv/after-file-open)))
                                         (eval `(defun ,close-sym ()
                                                  (interactive)
                                                  (kill-buffer ,x)
                                                  (vemv/clean-chosen-file-buffer-order)))
                                         (propertize shortname 'local-map `(keymap
                                                                            (mode-line keymap
                                                                                       (mouse-1 . ,sym)
                                                                                       (mouse-3 . ,close-sym))))))
                           rest))
         (p (propertize first 'face 'font-lock-function-name-face))
         (sep (propertize " | " 'face 'font-lock-line-and-column-face))
         (all (cons p the-rest)))
        (apply 'concat (-interpose sep all))))

(defun vemv/current-main-buffer-is-cljs ()
  (or (vemv/contains? (buffer-name) ".cljs")
      (vemv/contains? (buffer-name) ".cljc")))

(setq vemv/file-buffer-fallback "*scratch*")

(defun vemv/next-file-buffer ()
  "Switch to the next buffer that contains a file opened by the user."
  (interactive)
  (when (vemv/good-frame-p)
    (vemv/safe-select-window vemv/main_window))
  (vemv/clean-chosen-file-buffer-order)
  (switch-to-buffer (or (second vemv/chosen-file-buffer-order)
                        (first vemv/chosen-file-buffer-order)
                        vemv/file-buffer-fallback))
  (setq vemv/chosen-file-buffer-order `(,@(cdr vemv/chosen-file-buffer-order) ,(car vemv/chosen-file-buffer-order))))

(defun vemv/previous-file-buffer ()
  "Switch to the previous buffer that contains a file opened by the user."
  (interactive)
  (when (vemv/good-frame-p)
    (vemv/safe-select-window vemv/main_window))
  (vemv/clean-chosen-file-buffer-order)
  (if-let (file (or (car (last vemv/chosen-file-buffer-order)) (first vemv/chosen-file-buffer-order)))
    (progn
     (switch-to-buffer file)
     (setq vemv/chosen-file-buffer-order `(file ,@(butlast vemv/chosen-file-buffer-order))))
    (switch-to-buffer vemv/file-buffer-fallback)))

(defun vemv/home ()
  "Moves the point to leftmost non-empty character in the current line."
  (interactive)
  (move-beginning-of-line 1)
  (if (not (equal last-command 'vemv/home))
    (while (some (lambda (char) (equal char (vemv/current-char-at-point)))
                 '(" " "\t"))
      (forward-char))))

(defun vemv/end () ;; XXX doesn't honor region
  "Moves the point to rightmost non-empty character in the current line.

Comments get ignored, this is, point will only move as long as its position still belongs to the code - unless this command has been fired for the second time."
  (interactive)
  (if (equal last-command 'vemv/end)
    (call-interactively 'move-end-of-line)
    (let* ((line (vemv/current-line-contents))
           (rev (vemv/reverse line))
           (line_length (length line))
           (movement (recur-let ((result 0))
                                (if (some (lambda (char) (equal char (substring line result (inc result))))
                                          '(";" "\n"))
                                  result
                                  (recur (inc result))))))
          (move-beginning-of-line 1)
          (forward-char movement)
        ;; there may exist empty space between code and comment:
          (if (pos? movement)
            (while (not (some (lambda (char) (equal char (vemv/current-char-at-point)))
                              '(" ")))
              (backward-char)))
          (comm backward-char (recur-let ((result 0))
                                         (if (or
                                              (equal result line_length)
                                              (equal " " (substring rev result (inc result))))
                                           result
                                           (recur (inc result))))))))

(defun vemv/end-of-line-code ()
  (interactive "^")
  (save-match-data
   (let* ((bolpos (progn (beginning-of-line) (point)))
          (eolpos (progn (end-of-line) (point))))
         (if (comment-search-backward bolpos t)
           (search-backward-regexp comment-start-skip bolpos 'noerror))
         (skip-syntax-backward " " bolpos))))

(defun vemv/end-of-line-or-code ()
  (interactive "^")
  (ignore-errors (let ((here (point)))
                   (vemv/end-of-line-code)
                   (if (or (= here (point))
                           (bolp))
                     (end-of-line)))))

(defun vemv/line-empty? (line)
  (or (= 0 (length line))
      (every (lambda (char) (= char 32)) line)))

(defun vemv/delete-this-line ()
  "Deletes the entire current line regardless of its contents, and any preceding empty lines."
  (interactive)
  (end-of-line)
  (cua-set-mark)
  (previous-line)
  (end-of-line)
  (call-interactively 'kill-region)
  (let ((line (vemv/current-line)))
    (if (vemv/line-empty? line)
      (vemv/delete-this-line)
      (progn
       (next-line)
       (back-to-indentation)))))

(defun vemv/semicolon ()
  (interactive)
  (if (or (equal (vemv/current-char-at-point) ";")
          (progn "cursor is within string" nil)) ;; XXX
    (insert ";")
    (insert ";; "))) ;; (when (and (eolp) COLUMN > 0) (insert " "))

(setq vemv/shell-id 0)

(defun sh ()
  (interactive)
  (shell (concat "*shell-" (number-to-string (send! vemv/shell-id (lambda (a) (inc a)))) "*")))

(defun vemv/copy-selection-or-next-sexpr ()
  (let ((content (if (region-active-p)
                    (vemv/selected-region)
                    (vemv/sexpr-content))))
    (when (region-active-p)
      (call-interactively 'cua-set-mark))
    (vemv/bounded-list/insert-at-head! content vemv/kill-list vemv/kill-list-bound)
    (simpleclip-set-contents content)))

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
      (when (and (vemv/contains? (buffer-name b) ".clj")
                 (cider-connected-p))
        (vemv/save-position-before-formatting)
        (let ((old (substring-no-properties (buffer-string))))
          (save-excursion
            (condition-case nil (cider-format-buffer)
                            (error
                              (erase-buffer)
                              (insert old))))))
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
    (let* ((files (filter (lambda (x) (vemv/ends-with x ".cljs")) (directory-files-recursively "/Users/vemv/gpm/src/horizon/src/" ".cljs"))))
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

(defun vemv/load-clojure-buffer ()
  (interactive)
  (vemv/save)
  (vemv/save) ;; save autoformatting
  (vemv/advice-nrepl)
  (cider-load-buffer)
  (delay (argless (vemv/echo "Reloaded!") 0.1)))

(defun vemv/at-beginning-of-line-p ()
  (eq (point) (save-excursion (beginning-of-line) (point))))

(defun vemv/at-end-of-line-p ()
  (eq (point) (save-excursion (end-of-line) (point))))

(defun vemv/char-at-left ()
  (save-excursion
    (push-mark)
    (left-char)
   (vemv/selected-region)))

(defun vemv/chars-at-left ()
  (save-excursion
    (push-mark)
    (move-beginning-of-line 1)
   (vemv/selected-region)))

(defun vemv/char-at-right ()
  (save-excursion
    (push-mark)
    (right-char)
   (vemv/selected-region)))

(defun vemv/chars-at-right ()
  (save-excursion
    (push-mark)
    (move-end-of-line 1)
   (vemv/selected-region)))

(defmacro vemv/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
        ,@body
        (message "%.06f" (float-time (time-since time)))))

(defun vemv/initial-layout ()
 
 (if (window-system) (vemv/maximize))

 (split-window-vertically)
 (enlarge-window 8)

 (setq default-directory vemv-home)

 (let ((default-directory vemv/project-root-dir))
   (call-interactively 'project-explorer-open)
   (enlarge-window-horizontally -20)
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

(defun vemv/in-indentation-point-p ()
  "Whether the cursor is in a point apt for triggering an indentation command."
  
  (or (vemv/at-beginning-of-line-p)
      (every (lambda (x) (= x 32))
             (vemv/chars-at-left))))

(defun vemv/non-completable-char-p ()
  "Whether the cursor is in a point predictably impossible to autocomplete"
  (let ((current-char (vemv/current-char-at-point)))
    (-any?
      (lambda (x)
        (string-equal x current-char))
      (list "(" "[" "{" "#" "\""))))

(defun vemv/close-this-buffer ()
  (setq-local vemv/ns-shown nil)
  (kill-buffer (current-buffer))
  (when (and (eq (selected-window) vemv/main_window)
             (not (vemv/contains? (buffer-name) ".clj")))
    (vemv/next-file-buffer)))

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
         (list ".clj")))

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
  (vemv/stop-using-minibuffer) ;; For when minibuffer gets stuck (asks for input, but minibuffer-frame is in a different buffer from the current one)
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
  (if-let (unrelated-window (first (filter (lambda (w) (not (seq-contains (list vemv/repl2 vemv/project-explorer-window vemv/main_window)
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
          (-clone vemv/chosen-file-buffer-order))
  (switch-to-buffer "*scratch*"))

(defun vemv/close-all-other-file-buffers ()
  (interactive)
  (let ((root (buffer-name (current-buffer))))
    (mapcar (lambda (b)
              (unless (string-equal b root)
                (with-current-buffer b
                  (vemv/close-this-buffer))))
            (-clone vemv/chosen-file-buffer-order))))

(defun vemv/indent-on-paste ()
  (when (vemv/ciderable-p) ;; in-clojure-mode-p
      (paredit-backward)
      (vemv/indent)))

(defun vemv/clojure-init ()
  (if (minibuffer-prompt)
    (delay 'vemv/clojure-init 1)
    
    (advice-add 'pe/show-buffer :after 'vemv/after-file-open)
    (advice-add 'vemv/paste-from-clipboard :after 'vemv/indent-on-paste)
    (advice-add 'vemv/paste-from-kill-list :after 'vemv/indent-on-paste)
    (advice-add 'vemv/fiplr :after 'vemv/after-file-open)
    (advice-add 'vemv/open :after 'vemv/after-file-open)
    (advice-add 'vemv/next-file-buffer :after 'vemv/after-file-open)
    (advice-add 'vemv/previous-file-buffer :after 'vemv/after-file-open)
    (advice-add 'vemv/close-this-buffer :after 'vemv/after-file-open)
    (advice-add 'helm-ag--action-find-file :after 'vemv/after-file-open)
    
    (vemv/safe-select-window vemv/main_window)
    
    (if (file-readable-p recentf-save-file)
     (if (pos? (length recentf-list))
       (let* ((head (car recentf-list))
              (the-file (ignore-errors
                         (if (vemv/ends-with head "ido.last")
                           (second recentf-list)
                           head))))
             (when the-file
               (vemv/open
                (if (vemv/contains? (file-truename the-file) vemv/project-clojure-dir) ;; ensure nrepl opens a clojure context
                  the-file
                  vemv/default-clojure-file))
               (delay (argless (funcall vemv/safe-show-current-file-in-project-explorer))
                      3)))))))

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
  (if (and (not cider-launched) vemv/using-nrepl)
    (progn
     (setq cider-launched vemv/current-project)
     (setq vemv-cider-connecting t)
     (setq vemv/running-project vemv/current-project)
     (setq vemv/running-project-root-dir vemv/project-root-dir)
     (setq vemv/running-project-type vemv/project-type)
     (delay (argless (funcall vemv/project-initializers)
                     (select-window vemv/main_window)
                     (if (eq vemv/project-type :cljs)
                       (cider-jack-in-clojurescript)
                       (cider-jack-in)))
            1))
    (if vemv/using-nrepl
      (if (cider-connected-p)
        (if (vemv/current-main-buffer-is-cljs)
          (vemv/send :cljs)
          (vemv/send :clj)))
      (vemv/send :shell))))

(setq vemv/cider-prompt-for-symbol cider-prompt-for-symbol)

(defun vemv/jump-to-clojure-definition ()
  (interactive)
  (let* ((curr-token (cider-symbol-at-point 'look-back))
         (curr-token-is-qualified-kw (vemv/starts-with curr-token "::")))
      (setq cider-prompt-for-symbol nil)
      (if curr-token-is-qualified-kw
        (call-interactively 'cider-find-keyword)
        (cider-find-var))
      (setq cider-prompt-for-symbol vemv/cider-prompt-for-symbol)
      (vemv/advice-nrepl)))

(defun vemv/search-in-this-buffer ()
  (ignore-errors
   (call-interactively 'search-forward)
   (setq vemv-last-search (first minibuffer-history))))

(defun vemv/backspace ()
  (interactive)
  (if (region-active-p)
    (progn (call-interactively 'kill-region)
           (pop kill-ring))
    (paredit-backward-delete)))

(defun vemv/force-backspace ()
  "Performs a deletion, overriding paredit safeguards"
  (interactive)
  (if (region-active-p)
    (progn (call-interactively 'kill-region))
    (delete-region (dec (point)) (point))))

(defun vemv/open-file-via-fiplr-then-close-previous-buffer ()
  (interactive)
  (setq vemv/previous-buffer (current-buffer))
  (vemv/fiplr (lambda (filename)
                      (find-file filename)
                      (when (not (eq vemv/previous-buffer (current-buffer)))
                        (kill-buffer vemv/previous-buffer)))))

(defun vemv/new-frame ()
  (interactive)
  (make-frame `((width . ,(frame-width)) (height . ,(frame-height))))) ;; in order to kill a frame, use the window system's standard exit (e.g. Alt-F4) command. The other frames won't close

(defun vemv/repeat-last-search-in-this-buffer ()
  (interactive)
  (ignore-errors (search-forward vemv-last-search)))
  
(defun vemv/smex ()
  (when vemv/launched (smex)))
  
(defun vemv/copy-sexpr-content-backward ()
  (interactive)
  (kill-new (vemv/sexpr-content :backward)))

(defun vemv/kill-backward ()
  (interactive)
  (vemv/kill :backward))

(defun vemv/kill-backward-copying-content ()
  (interactive)
  (kill-new (vemv/kill :backward)))

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

(defmacro vemv/save-window-excursion (&rest forms)
  `(let ((current-window (selected-window)))
     (save-excursion
       ,@forms)
     (vemv/safe-select-window current-window)))

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

(defun vemv/test-this-ns ()
  "Runs the tests for the current namespace if its name contains 'test', or the latest ns that did."
  (interactive)
  (vemv/advice-nrepl (argless
                      (let* ((cljs (vemv/current-main-buffer-is-cljs))
                            (ns (vemv/current-ns))
                            (chosen (if (vemv/contains? ns "test") ns (if cljs vemv/latest-cljs-test-ran vemv/latest-clojure-test-ran))))
                        (when chosen
                          (setq vemv/latest-clojure-test-ran chosen)
                          (vemv/send (if cljs :cljs :clj)
                                     nil
                                     (concat (if (and cljs (vemv/contains? (vemv/current-ns) "smoke"))
                                                 "(.reload js/location true) "
                                                 "")
                                             "(cljs.test/run-tests '"
                                             chosen
                                             ")")))))))

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

(defun vemv/paste-from-clipboard ()
  (insert (substring-no-properties (simpleclip-get-contents))))

(defun vemv/paste-from-kill-list ()
  (insert (car vemv/kill-list)))

(defun vemv/onelineize ()
  "Turns the current sexpr into a oneliner"
  (let ((replacement (replace-regexp-in-string "[\s|\n]+" " " (vemv/sexpr-content))))
    (vemv/kill)
    (insert (concat replacement " "))
    (when (string-equal " " (vemv/char-at-left))
      (paredit-backward-delete))
    (call-interactively 'paredit-backward)))

(defun vemv/emacs-reload ()
  (load "vemv.project")
  (load "vemv.lang")
  (load "vemv.data.bindings")
  (load "vemv.shortcuts.global.base")
  (load "vemv.shortcuts.global")
  (load "vemv.shortcuts.clojure")
  (load "vemv.theme"))

(defun vemv/next-project ()
  (interactive)
  (setq vemv/all-projects `(,@(cdr vemv/all-projects) ,(car vemv/all-projects)))
  (vemv/refresh-current-project (car vemv/all-projects) :switch))

(defun vemv/previous-project ()
  (interactive)
  (setq vemv/all-projects `(,(or (car (last vemv/all-projects)) (first vemv/all-projects))
                            ,@(butlast vemv/all-projects)))
  (vemv/refresh-current-project (car vemv/all-projects) :switch))

(defun vemv/replace-regexp-entire-buffer (pattern replacement)
  "Perform regular-expression replacement throughout buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))

;; `-u foo`: case-sensitive
;; `-i foo`: case-insensitive
;; Pasting must be done with tertiary-v
;; C-S: mark occurrence
;; RET: for choosing where to search
;; TAB: generally never use.
;; RET: edit marked ocurrences (or all, by default)
;; S-RET: goto file
(defun vemv/ag-replace (&rest _)
  (interactive)
  (with-current-buffer "*helm-ag-edit*"
    (let* ((search (replace-regexp-in-string "^\-[u|i] " "" helm-ag--last-query))
          (replacement (read-from-minibuffer (concat "Replacement for `" search "`: "))))
      (when (and replacement (not (string-equal replacement "")))
        (vemv/replace-regexp-entire-buffer search replacement)
        (call-interactively 'helm-ag--edit-commit)
        (vemv/echo "Replaced!")))))

(defun vemv/abort-ag ()
  (interactive)
  (vemv/stop-using-minibuffer 'helm-ag--exit-from-edit-mode))

;; redefine for having just one action

(setq helm-ag--actions
  (helm-make-actions
   "Edit search results" #'helm-ag--edit
   "Open file"           #'helm-ag--action-find-file))

;; redefine for using the redefined helm-ag--actions

(setq helm-ag-source
  (helm-build-in-buffer-source "The Silver Searcher"
    :init 'helm-ag--init
    :real-to-display 'helm-ag--candidate-transformer
    :persistent-action 'helm-ag--persistent-action
    :fuzzy-match helm-ag-fuzzy-match
    :action helm-ag--actions
    :candidate-number-limit 9999
    :keymap helm-ag-map
    :follow (and helm-follow-mode-persistent 1)))
  
;; same

(setq helm-source-do-ag
  (helm-build-async-source "The Silver Searcher"
    :init 'helm-ag--do-ag-set-command
    :candidates-process 'helm-ag--do-ag-candidate-process
    :persistent-action  'helm-ag--persistent-action
    :action helm-ag--actions
    :nohighlight t
    :requires-pattern 3
    :candidate-number-limit 9999
    :keymap helm-do-ag-map
    :follow (and helm-follow-mode-persistent 1)))
