;; -*- lexical-binding: t; -*-

(require 'recur)
(require 'multi-methods)
(provide 'vemv.lang)

;; elisp gotchas: let vs. let* · last returns a list · "Wrong type argument: commandp" -> forgot interactive

(defmacro comm (&rest forms)
  "Comment. Doesn't evaluate its arguments, returns nil.")

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

(defmacro if-not (test &rest forms))

(defmacro when-not (test &rest forms))

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

(defun vemv/selected-window-number ()
  (- (string-to-int (window-number-string))))

(defun vemv/window-number-of-buffer (buffer-or-name)
  "XXX")

(defun vemv/selected-region ()
  "Returns the selected region as a string. Side effects free."
  (kill-ring-save (mark) (point))
  (let ((result (substring-no-properties (car kill-ring))))
    (pop kill-ring)
    result))

(defun vemv/current-frame-buffers ()
  (mapcar #'buffer-name (mapcar #'window-buffer (window-list))))

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

(setq vemv/clj-repl-name (concat "*cider-repl " vemv/project-ns-prefix "*"))
(setq vemv/cljs-repl-name (concat "*cider-repl CLJS " vemv/project-ns-prefix "*"))
(setq cider-launched nil)

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
      (let ((sender (selected-window)))
        (vemv/safe-select-window vemv/repl2)
        (vemv/switch-to-buffer-in-any-frame (case where
                                              (:cider the-cider-buffer-name)
                                              (:ielm "*ielm*")
                                              (:shell "*shell-1*")
                                              (:clj vemv/clj-repl-name)
                                              (:cljs vemv/cljs-repl-name)))

        (end-of-buffer)
        (insert content)

        (case where
          (:cider (cider-repl-return))
          (:ielm (ielm-return))
          (:shell (comint-send-input))
          (:clj (cider-repl-return))
          (:cljs (cider-repl-return)))

        (pop kill-ring)
        (end-of-buffer)
        (vemv/safe-select-window sender)))))

; XXX infer whether the user wants to insert newlines
(defun vemv/duplicate (&optional backward?) ;; XXX indentation: stuff gets inserted at the absolute beggining of line TODO backward?, for sexprs
  "Copies the current line (or sexpr, if point is at the beggining of one, or selection, if the region is active), inserting it at a new line."
  (interactive)

  (if (region-active-p)

    (progn
     (dotimes (i (- (region-end) (point)))
       (forward-char))
     (insert "\n" (vemv/selected-region) "\n"))

    (if (some (lambda (char) (equal char (vemv/current-char-at-point)))
              '("(" "[" "{" "<" "\""))
      (progn
       (let ((content (vemv/sexpr-content))
             (whitespace (progn (comm let N the num of chars until beggining-of-line, N* " ") "")))
         (paredit-forward)
         (insert (concat "\n\n" whitespace content))
         (call-interactively 'move-end-of-line) ;; XXX end of sexpr instead
         (paredit-backward)))

      (progn
       (move-beginning-of-line 1)
       (kill-line)
       (yank)
       (open-line 1)
       (next-line 1)
       (yank)
       (pop kill-ring)))))

(defun vemv/kill (&optional backward?) ;; XXX kill comments FIXME can leave sexprs unmatched
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
     result)))

(defun vemv/delete-backward (&optional cut?)
  "Performs a paredit-backward-delete unless the region is active, in which case the selection gets unconditionally removed.

The removed value will be pushed to the kill-ring only on non-nil values of CUT?.

Unconditionally removing code may yield semantically wrong results, i.e. leaving sexprs unmatched. I personally like this tradeoff - use with caution!"
  (interactive)

  (if (region-active-p)
    (progn (call-interactively 'kill-region)
           (if (not cut?) (pop kill-ring)))
    (paredit-backward-delete)))

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

(defun vemv/elisp-popup-documentation ()
  "Pops up the documentation for the symbol that is currently hovered by the point. Presumes emacs-lisp-mode."
  (interactive)
  (if-let (f (function-called-at-point))
    (let ((string (ac-symbol-documentation f)))
      (cond
        ((and window-system (featurep 'pos-tip)) ;; see: `ac-pos-tip-show-quick-help'
         (pos-tip-show string 'popup-tip-face nil nil 0 popup-tip-max-width))
        ((featurep 'popup)
         (popup-tip string :height ac-quick-help-height))
        (t
         (message string))))))

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

(defun vemv/current-line-content ()
  "Returns the content of the line at which the point is currently located. Side effects free."
  (interactive)
  (let ((result (buffer-substring-no-properties (line-beginning-position 1) (line-beginning-position 2))))
    (if (equal result "") ;; abstact away EOFs
      "\n"
      result)))

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

(defun vemv/show-current-file-in-project-explorer ()
  (interactive)
  (if (minibuffer-prompt)
    (delay 'vemv/show-current-file-in-project-explorer 1)

    (vemv/refresh-file-caches)
    (vemv/safe-select-window vemv/main_window)
    (let* ((buffer-fragments (-remove (lambda (x) (string-equal x "")) (split-string (buffer-file-name) "/")))
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

          (end-of-line))))

(defun vemv/safe-show-current-file-in-project-explorer ()
  (condition-case nil
                  (vemv/show-current-file-in-project-explorer)
                  (error (ignore-errors (vemv/show-current-file-in-project-explorer))))
  (vemv/safe-select-window vemv/main_window))

(defun vemv/current-ns (&optional which-buffer)
  (with-current-buffer (buffer-name which-buffer)
    (cider-current-ns)))

(defun vemv/advice-nrepl ()
  (interactive)
  (delay (argless
          (when (and (vemv/contains? (buffer-name) ".clj")
                     (cider-connected-p)
                     (not (string-equal (vemv/current-ns)
                                        (vemv/current-ns (window-buffer vemv/repl2)))))
            (cider-repl-set-ns (vemv/current-ns))))
         1))

(defun vemv/toggle-ns-hiding ()
  (interactive)
  (when (not vemv-cleaning-namespaces)
    (setq-local vemv/ns-hidden (not vemv/ns-hidden))
    (if vemv/ns-hidden
      (let* ((hs-block-start-regexp "(ns")
             (hs-block-end-regexp ")")
             (hs-hide-comments-when-hiding-all nil)
             (hs-adjust-block-beginning (lambda (initial)
                                                (save-excursion
                                                 (point)))))
            (apply #'hs-hide-all ()))
      (hs-show-all))))

(defun vemv/show-clj-or-cljs-repl ()
  (vemv/safe-select-window vemv/main_window)
  (setq was (vemv/current-main-buffer-is-cljs))
  (vemv/safe-select-window vemv/repl2)
  (if was
    (switch-to-buffer vemv/cljs-repl-name)
    (switch-to-buffer vemv/clj-repl-name))
  (vemv/safe-select-window vemv/main_window))

(defun vemv/ensure-repl-visible ()
  (when (cider-connected-p)
    (vemv/show-clj-or-cljs-repl)))

(defun vemv/after-file-open (&rest ignore)
  (interactive)
  (vemv/safe-select-window vemv/main_window)
  (when (and (vemv/contains? (buffer-name) ".clj")
             (not vemv/ns-hidden))
    (vemv/toggle-ns-hiding))
  (vemv/advice-nrepl)
  (vemv/ensure-repl-visible)
  (delay 'vemv/safe-show-current-file-in-project-explorer 0.1))

(defun vemv/open_file_buffers ()
  (let ((c (mapcar (lambda (x) (buffer-name x)) (buffer-list))))
    (filter (lambda (filename) (vemv/contains? filename ".clj")) c)))

(setq vemv/chosen-file-buffer-order nil) ;; a list

(defun vemv/clean-chosen-file-buffer-order ()
  (let* ((curr (buffer-name (current-buffer)))
         (c (vemv/open_file_buffers))
         (all (-distinct (-concat vemv/chosen-file-buffer-order c)))
         (without-curr (-remove (lambda (x) (string-equal x curr)) all))
         (final (cons curr without-curr)))
        (setq vemv/chosen-file-buffer-order (filter (lambda (x)
                                                            (member x c))
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
  (vemv/safe-select-window vemv/main_window)
  (vemv/clean-chosen-file-buffer-order)
  (switch-to-buffer (or (second vemv/chosen-file-buffer-order)
                        (first vemv/chosen-file-buffer-order)
                        vemv/file-buffer-fallback))
  (setq vemv/chosen-file-buffer-order `(,@(cdr vemv/chosen-file-buffer-order) ,(car vemv/chosen-file-buffer-order))))

(defun vemv/previous-file-buffer ()
  "Switch to the previous buffer that contains a file opened by the user."
  (interactive)
  (vemv/safe-select-window vemv/main_window)
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
    (let* ((line (vemv/current-line-content))
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
  (cua-set-mark)
  (previous-line)
  (end-of-line)
  (call-interactively 'kill-region)
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
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
  (if (region-active-p)
    (call-interactively 'kill-ring-save)
    (kill-new (vemv/sexpr-content))))

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

(defun vemv/save ()
  (interactive)
  (save-buffer)
  (when (and (vemv/contains? (buffer-name (current-buffer)) ".clj")
             (cider-connected-p))
    (save-excursion (cider-format-buffer)))
  (when (buffer-modified-p)
    (vemv/echo "Formatted!")))

(setq vemv/ns-hidden nil)

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
  (cider-load-buffer))

(defun vemv/at-beginning-of-line-p ()
  (eq (point) (save-excursion (beginning-of-line) (point))))

(defun vemv/at-end-of-line-p ()
  (eq (point) (save-excursion (end-of-line) (point))))

(defun vemv/chars-at-left ()
  (save-excursion
    (push-mark)
    (move-beginning-of-line 1)
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
      (list "(" "[" "{" "}" "]" ")" ";" "#" "\""))))

(defun vemv/close-this-buffer ()
  (interactive)
  (setq-local vemv/ns-hidden nil)
  (kill-buffer (current-buffer))
  (unless (vemv/contains? (buffer-name) ".clj")
          (vemv/next-file-buffer)))

(defun vemv/good-buffer-p ()
  (-any? (lambda (x) (vemv/contains? (buffer-name) x))
         (list ".clj"
               vemv/clj-repl-name
               vemv/cljs-repl-name
               "project-explorer"
               "shell-1"
               "scratch")))

(defun vemv/good-window-p ()
  (or (eq (selected-window) vemv/main_window)
      (eq (selected-window) vemv/repl2)
      (eq (selected-window) vemv/project-explorer-window)))

(setq vemv/main_frame (selected-frame))

(defun vemv/good-frame-p ()
  (eq vemv/main_frame (selected-frame)))

(defun vemv/close-this-buffer ()
  (setq-local vemv/ns-hidden nil)
  (kill-buffer (current-buffer))
  (when (and (eq (selected-window) vemv/main_window)
             (not (vemv/contains? (buffer-name) ".clj")))
        (vemv/next-file-buffer)))

(defun vemv/close-this-window ()
  (delete-window))

(defun vemv/close-this-frame ()
  (delete-frame (selected-frame) t))

(defun vemv/close-this ()
  (interactive)
  (if (or (not (vemv/good-buffer-p))
          (and (vemv/good-buffer-p) (vemv/good-window-p)))
    (vemv/close-this-buffer))
  (unless (< (length (vemv/current-frame-buffers)) 2)
    (unless (vemv/good-window-p)
      (vemv/close-this-window)))
  (unless (vemv/good-frame-p)
    (vemv/close-this-frame)))

(defun vemv/clojure-init ()
  (if (minibuffer-prompt)
    (delay 'vemv/clojure-init 1)
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
                (if (vemv/contains? the-file vemv/project-clojure-dir) ;; ensure nrepl opens a clojure context
                  the-file
                  vemv/default-clojure-file))
               (delay 'vemv/safe-show-current-file-in-project-explorer 3)))))

    (advice-add 'pe/show-buffer :after 'vemv/after-file-open)
    (advice-add 'vemv/fiplr :after 'vemv/after-file-open)
    (advice-add 'vemv/open :after 'vemv/after-file-open)
    (advice-add 'vemv/next-file-buffer :after 'vemv/after-file-open)
    (advice-add 'vemv/previous-file-buffer :after 'vemv/after-file-open)
    (advice-add 'vemv/close-this-buffer :after 'vemv/after-file-open)))
