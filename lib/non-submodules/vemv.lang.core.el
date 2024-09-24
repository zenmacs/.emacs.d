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

(defmacro replying-yes (&rest forms)
  `(cl-flet ((always-yes (&rest _) t))
     (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
               ((symbol-function 'yes-or-no-p) #'always-yes))
       ,@forms)))

;; Works around `max-mini-window-height', which Emacs doesn't always honor
(defvar vemv/max-mini-window-height nil)

(defun vemv/force-concat (&rest xs)
  (->> xs
       (mapcar (lambda (x)
                 (if (stringp x)
                     x
                   (pr-str x))))
       (-interpose " ")
       (apply 'concat)))

(defun vemv/echo (&rest xs)
  (setq max-mini-window-height (or vemv/max-mini-window-height max-mini-window-height))
  (let ((what (apply 'vemv/force-concat xs)))
    (vemv/verbosely
     (message "%s" what))
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

(defun vemv/largest-unselected-window (window1 window2)
  "Return the largest unselected window between WINDOW1 and WINDOW2."
  (let ((selected-window (selected-window))
        (area1 (* (window-width window1) (window-height window1)))
        (area2 (* (window-width window2) (window-height window2))))
    (cond
     ;; If WINDOW1 is selected or WINDOW1 and WINDOW2 are the same, return WINDOW2 (if not selected).
     ((or (eq window1 selected-window) (eq window1 window2))
      (unless (eq window2 selected-window) window2))
     ;; If WINDOW2 is selected, return WINDOW1 (if not selected).
     ((eq window2 selected-window)
      window1)
     ;; If neither window is selected, return the one with the larger area.
     ((> area1 area2) window1)
     (t window2))))

(defun vemv/assign-largest-unselected-window (buffer)
  (let* ((window (vemv/largest-unselected-window vemv/main_window vemv/repl-window)))
    ;; xxx if win is too small, enlarge it
    (vemv/safe-select-window window)
    (set-window-buffer window buffer)))

;; display-buffer-use-some-window
(defun vemv/ensure-other-frame (buffer _alist)
  (let* ((in-main-frame? (equal vemv/main_frame
                                (window-frame (get-buffer-window (current-buffer)))))
         (frame (if in-main-frame?
                    (vemv/new-frame :no-show :no-margin)
                  (selected-frame)))
         (window (frame-selected-window frame)))
    (set-window-buffer window buffer)
    (when (vemv/contains? (buffer-name buffer) "magit:")
      (setq vemv/magit_frame frame))
    (modify-frame-parameters frame '((visibility . t)))
    (select-frame-set-input-focus frame)))

(defun vemv/display-completion (buffer)
  (vemv/safe-select-window vemv/main_window)
  (set-window-buffer vemv/main_window buffer))

(defun vemv/assign-current-window (buffer)
  (set-window-buffer (get-buffer-window (current-buffer))
                     buffer))

(defun vemv/assign-inspector-window (buffer)
  (let ((v (set-window-buffer (frame-first-window vemv/inspector_frame)
                              buffer)))
    (select-frame-set-input-focus vemv/inspector_frame)
    v))

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

(require 'whitespace)
(define-global-minor-mode vemv/global-whitespace-mode whitespace-mode
  (lambda ()
    (when (and (buffer-file-name)
               whitespace-line-column
               ;; https://github.com/magit/magit/issues/4766#issue-1379227683
               (not (derived-mode-p 'magit-mode)))
      (whitespace-mode 1)))
  :group 'whitespace)
