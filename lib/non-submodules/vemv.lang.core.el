;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.lang.core)

(defmacro comm (&rest forms)
  "Comment. Doesn't evaluate its arguments, returns nil."
  nil)

(defmacro argless (&rest forms)
  "Shortcut for (lambda () (interactive) ,@forms)"
  `(lambda (&rest _)
     (interactive)
     ,@forms))

(defmacro if-let (binding &rest forms)
  "Usage: (if-let (x (some-computation))
                (then x)
                (else x) (else_2) ... (else_N))"
  (let ((symbol (first binding))
        (value (second binding)))
    `(let ((,symbol ,value))
       (if ,symbol
           ,@forms))))

(defmacro replying-yes (&rest forms)
  `(cl-flet ((always-yes (&rest _) t))
     (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
               ((symbol-function 'yes-or-no-p) #'always-yes))
       ,@forms)))

;; Works around `max-mini-window-height', which Emacs doesn't always honor
(defvar vemv/max-mini-window-height nil)

(defun vemv/echo (&rest xs)
  (setq max-mini-window-height (or vemv/max-mini-window-height max-mini-window-height))
  (let ((what (->> xs
                   (mapcar (lambda (x)
                             (if (stringp x)
                                 x
                               (pr-str x))))
                   (-interpose " ")
                   (apply 'concat))))
    (vemv/verbosely
     (message what))
    (setq vemv/max-mini-window-height nil)
    what))

(defun pr-str (x)
  (prin1-to-string x))

(defun delay (f &optional seconds)
  "Calls f in one or SECONDS seconds."
  (-> (or seconds 1)
      (int-to-string)
      (concat " seconds")
      (run-at-time nil f)))

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

(defun vemv/drop (n seq)
  (-drop n seq))

(defun vemv/take (n seq)
  (-take n seq))

(defun vemv/partition (n s)
  (-partition n s))

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
      `(lambda (&rest args)
         (progn
           (if
               (and (vectorp ,timer) (not (aref ,timer 0)))
               (cancel-timer ,timer))
           (setq
            ,timer
            (run-at-time
             ,delay nil
             (lambda (params)
               (apply ,callee params))
             args)))))))

(defun vemv/contains? (a b)
  "Whether the string B is contained in A."
  (and a b (s-contains? b a)))

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
  (s-ends-with? ending s))

(defun vemv/starts-with (s candidate)
  "Returns non-nil if string S starts with CANDIDATE."
  (let ((clength (length candidate)))
    (if (<= clength (length s))
        (string= (substring s 0 clength) candidate))))

(defun vemv/keyword-to-string (arg)
  ":foo -> \"foo\""
  (substring (symbol-name arg) 1))

(defun vemv/timestamp ()
  (truncate (float-time)))

(defun vemv/scratch-p ()
  (string-equal "*scratch*" (buffer-name (current-buffer))))

(defmacro vemv/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun vemv/keyboard-macro (key)
  (if (stringp key)
      (read-kbd-macro key)
    key))

(defun vemv/display-completion (buffer)
  (vemv/safe-select-window vemv/main_window)
  (set-window-buffer vemv/main_window buffer))

(defun vemv/repl-completion (buffer)
  (vemv/safe-select-window vemv/repl-window)
  (set-window-buffer vemv/repl-window buffer))

(defun vemv.completions/split-window-vertically-impl (buffer &optional big)
  (vemv/safe-select-window vemv/repl-window)
  (enlarge-window -1000)
  (vemv/safe-select-window vemv/main_window)
  (let* ((total (frame-total-lines))
         (middle (/ total 2))
         (repl-height 4)
         (x (-> middle
                (- repl-height)
                (/ (if big 1 2))
                (- 1)
                (-))))
    (set-window-buffer (split-window-vertically x) buffer)))

(defun vemv.completions/split-window-vertically-small (buffer)
  (vemv.completions/split-window-vertically-impl buffer))

(defun vemv.completions/split-window-vertically-big (buffer)
  (vemv.completions/split-window-vertically-impl buffer :big))

(defun vemv.completions/in-new-frame (buffer)
  (let* ((f (vemv/new-frame)))
    (select-frame f)
    (set-window-buffer (car (window-list f)) buffer)))

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

(defun vemv/hash-map-to-list (hash-table)
  (let (result)
    (maphash (lambda (k v)
               (push (list k v) result))
             hash-table)
    result))

(defun vemv/read-from-minibuffer (&optional prompt)
  "Catches C-g as nil"
  (interactive)
  (let* ((inhibit-quit t)
         (output (with-local-quit
                   (read-from-minibuffer (or prompt "=> ")))))
    (or output
        (setq quit-flag nil))))

(defun vemv/fontify ()
  "Performs the font-lock operations that must be executed after initializing a mode."
  ;; Boilerplate copied from paren-face-mode
  (when font-lock-mode
    (if (and (fboundp 'font-lock-flush)
             (fboundp 'font-lock-ensure))
        (save-restriction
          (widen)
          (font-lock-flush)
          (font-lock-ensure))
      (with-no-warnings
        (font-lock-fontify-buffer)))))
