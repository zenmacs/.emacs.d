;; -*- lexical-binding: t; -*-

(require 'recur)
(require 'multi-methods)
(provide 'vemv.lang.core)

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

(defmacro replying-yes (&rest forms)
  `(cl-flet ((always-yes (&rest _) t))
     (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
               ((symbol-function 'yes-or-no-p) #'always-yes))
       ,@forms)))

(defmacro vemv/verbosely (&rest forms)
  `(let ((old vemv/verbose-mode))
     (vemv/set-verbosity-to t)
     ,@forms
     (vemv/set-verbosity-to old)))

(defun vemv/apply-verbosely (f &rest args)
  (vemv/verbosely
   (apply f args)))

(defun vemv/echo (&rest xs)
  (let ((what (apply 'concat xs)))
    (setq inhibit-message nil)
    (message what)
    (setq inhibit-message t)
    what))

(defun pr-str (x)
  (prin1-to-string x))

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
  "Divides SEQ in a list of lists of N items each, at offsets STEP or N apart.
ACC is an implementation detail - do not pass this parameter!"
  (if seq
      (recur n ;; XXX recur takes the args in mistaken order. wut
             (vemv/drop (or step n) seq)
             (if-let (taken (vemv/take n seq))
                 (cons taken acc)
               acc)
             (or step n))
      (reverse acc)))

(defun vemv/in-a-lisp-mode? ()
  (or (eq major-mode 'emacs-lisp-mode)
      (eq major-mode 'clojure-mode)
      (eq major-mode 'inferior-emacs-lisp-mode)))

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

(defun vemv/safe-select-window (x)
  (unless (minibuffer-prompt)
    (select-window x)))

(defmacro vemv/save-window-excursion (&rest forms)
  `(let ((current-window (selected-window))
         (v (save-excursion
              ,@forms)))
     (vemv/safe-select-window current-window)
     v))

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

(defun vemv/previous-line ()
  (save-excursion
    (call-interactively 'previous-line)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

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

(defun vemv/in-clojure-mode? ()
  ;; better: derived-mode-p
  (vemv/contains? (pr-str major-mode) "clojure"))

(defun vemv/ciderable-p ()
  (and
   (vemv/in-clojure-mode?)
   (cider-connected-p)
   vemv-cider-connected))

(defun vemv/timestamp ()
  (truncate (float-time)))

(defun vemv/scratch-p ()
  (string-equal "*scratch*" (buffer-name (current-buffer))))

(defun vemv/current-main-buffer-is-cljs ()
  (or (vemv/contains? (buffer-name) ".cljs")
      (and (vemv/contains? (buffer-name) ".cljc")
           (eq vemv/project-type :cljs))))

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
    (if (and (vemv/line-empty? line)
             (vemv/line-empty? (vemv/previous-line)))
        (vemv/delete-this-line)
        (progn
          (next-line)
          (back-to-indentation)))))

(defun vemv/semicolon ()
  (interactive)
  (if (or (equal (vemv/current-char-at-point) ";")
          (progn "cursor is within string" nil)) ;; XXX
      (insert ";")
      (insert ";; ")))

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

(defun vemv/new-frame ()
  (interactive)
  ;; in order to kill a frame, use the window system's standard exit (e.g. Alt-F4) command. The other frames won't close
  (make-frame `((width . ,(frame-width)) (height . ,(frame-height)))))

(defun vemv/repeat-last-search-in-this-buffer ()
  (interactive)
  (ignore-errors (search-forward vemv-last-search)))

(defun vemv/keyboard-macro (key)
  (if (stringp key)
      (read-kbd-macro key)
      key))

(defun vemv/display-completion (buffer)
  (vemv/safe-select-window vemv/main_window)
  (set-window-buffer vemv/main_window buffer))

(defun vemv/replace-regexp-entire-buffer (pattern replacement)
  "Perform regular-expression replacement throughout buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))

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
