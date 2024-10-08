;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.paredit)

(setq vemv/kill-list-bound 10)

;; The 10 last elements copied to the clipboard.
;; I don't use kill-ring, since third-parties (e.g. paredit) can mess with it
(setq vemv/kill-list (-repeat vemv/kill-list-bound nil))

(defun vemv/save (&optional b skip-check-unused-requires skip-formatting avoid-recursion)
  (interactive)
  (let* ((bfn (buffer-file-name)))
    (when (and bfn
               (not (s-contains? ".jar:" bfn)))
      (let* ((b (or b (current-buffer))))
        (with-current-buffer b
          (when (vemv/in-a-lisp-mode?)
            (check-parens))
          (let* ((line (vemv/current-line-number))
                 ;; for `indent-for-tab-command`:
                 (last-command nil)
                 (dc (string-equal "dc" (car vemv/current-workspace)))
                 (clash? (or (s-contains? "/monorail" default-directory)))
                 (yas (vemv/contains? (buffer-file-name) "/snippets/")))
            (unless (or vemv.project/skip-formatting
                        skip-formatting
                        yas
                        (member major-mode `(fundamental-mode ruby-mode)))
              (unless dc
                (delete-trailing-whitespace))
              (unless (or clash?
                          (member major-mode `(conf-colon-mode sql-mode makefile-automake-mode makefile-bsdmake-mode)))
                (call-interactively 'mark-whole-buffer)
                (call-interactively 'indent-for-tab-command))
              (when (and clash?
                         (vemv/in-a-clojure-mode?))
                (save-excursion
                  (beginning-of-defun)
                  (vemv/indent))
                (beginning-of-line)
                (call-interactively 'indent-for-tab-command))
              (pop-mark))
            (goto-line line)
            (condition-case nil
                (vemv/end-of-line-code* nil)
              (error nil))
            (when (or vemv/no-newline-at-eof yas)
              (save-excursion
                (end-of-buffer)
                (while (string-equal (vemv/current-char-at-point) "\n")
                  (delete-backward-char 1))))

            (when (member major-mode `(typescript-mode))
              (set-buffer-modified-p t))
            (save-buffer)

            (when (eq major-mode 'ruby-mode)
              (unless skip-formatting
                (require 'rubocop)
                (defun rubocop--file-command (command)
                  "Removes compilation-mode stuff"
                  (rubocop-ensure-installed)
                  (let ((file-name (buffer-file-name (current-buffer))))
                    (if file-name
                        ;; make sure we run RuboCop from a project's root if the command is executed within a project
                        (let ((default-directory (or (rubocop-project-root 'no-error) default-directory)))
                          (shell-command-to-string (rubocop-build-command command (rubocop-local-file-name file-name)))
                          (revert-buffer t t t))
                      (error "Buffer is not visiting a file"))))
                (rubocop-autocorrect-current-file))
              (when vemv-robe-connected
                (vemv/send :ruby nil "$VERBOSE = nil; reload!")))
            (when (and (member major-mode `(typescript-mode))
                       (not avoid-recursion))
              (vemv/save-other-buffers-for-this-project :for-flycheck))
            (when (string-equal (buffer-file-name) vemv/overrides-file)
              (vemv/refresh-available-projects)
              (with-selected-window vemv/project-explorer-window
                (funcall vemv/maybe-change-project-graphically)))))))))

(defun vemv/tab ()
  (interactive)
  (or (and (or
            (vemv/in-indentation-point-p)
            (vemv/non-completable-char-p))
           (or (let ((last-command nil))
                 (call-interactively 'indent-for-tab-command))
               t))
      (and (not (vemv/non-completable-char-p))
           (call-interactively 'company-complete))
      (and (not (vemv/non-completable-char-p))
           ;; (call-interactively 'company-dabbrev)
           (progn
             (require 'company-yasnippet)
             (call-interactively 'company-yasnippet)))))

(defun vemv/dumb-cut ()
  "Cuts the current selection, regardless of paredit boundaries"
  (interactive)
  (when (region-active-p)
    (let ((content (vemv/selected-region)))
      (call-interactively 'kill-region)
      (vemv/bounded-list/insert-at-head! content
                                         vemv/kill-list
                                         vemv/kill-list-bound)
      (simpleclip-set-contents content))))

(defun vemv/cut ()
  "Cuts via vemv/kill, i.e. taking into account paredit boundaries"
  (interactive)
  (vemv/bounded-list/insert-at-head! (vemv/kill nil nil)
                                     vemv/kill-list
                                     vemv/kill-list-bound))

(defun vemv/copy-inserting-at-kill-list ()
  (interactive)
  (vemv/bounded-list/insert-at-head! (vemv/copy-selection-or-next-sexpr)
                                     vemv/kill-list
                                     vemv/kill-list-bound))

(defun vemv/maybe-indent-on-paste (content)
  (when (and (vemv/in-a-lisp-mode?)
             (s-match "^\s*[\(|[|{]" content))
    (paredit-backward)
    (vemv/indent)))

(defun vemv/before-paste ()
  (when (region-active-p)
    (kill-region (region-beginning) (region-end))))

(defun vemv/paste-from-clipboard ()
  (interactive)
  (vemv/before-paste)
  (let ((content (substring-no-properties (simpleclip-get-contents))))
    (insert content)
    (vemv/maybe-indent-on-paste content)))

(defun vemv/paste-from-kill-list ()
  (interactive)
  (vemv/before-paste)
  (let ((content (car vemv/kill-list)))
    (insert content)
    (vemv/maybe-indent-on-paste content)))

(defun vemv/pull-next-sexpr ()
  "Brings the sexpr located in the next line at the current one."
  (interactive)
  (just-one-space -1)
  (while (progn (let ((m (member (vemv/current-char-at-point) `("(" ")" "[" "]" "{" "}"))))
                  (when m
                    (right-char))
                  (let ((v (-find (lambda (x)
                                    (vemv/ends-with (vemv/chars-at-left) x))
                                  `(" )" " ]" " }"
                                    "(( " "[[ " "{{ "
                                    "( (" "[ [" "{ {"
                                    " ( " " [ " " { "))))
                    (when m
                      (left-char))
                    v)))
    (backward-delete-char 1)))

(defmacro vemv/paredit-almost-safely (&rest body)
  `(when (or (vemv/in-a-lisp-mode?)
             (derived-mode-p 'magit-mode))
     (let* ((last-command nil)
            (v (progn
                 ,@body)))
       v)))

(defmacro vemv/paredit-safely (&rest body)
  "* Paredit commands over non-lisps can cause Emacs freezes.
   * `comment-indent-function' should be the default, else `paredit-forward-slurp-sexp' can break sexprs.
   * Indentation can go wild due to `last-command'"
  `(let* ((comment-indent-function 'comment-indent-default)
          (v (vemv/paredit-almost-safely
              ,@body)))
     v))

(defun vemv/safe-paredit-command (command)
  (argless
   (vemv/paredit-safely
    (call-interactively command))))

(defun vemv/sexpr-content (&optional backward? with-properties?)
  "Returns the content of the next (or previous, on non-nil values of BACKWARD?) sexpr, as a string.

Unlike paredit-copy-as-kill, this function will only grab one sexpr (and no more even -
if they are contigous), and is side-effect free."
  (interactive)
  (save-excursion
    (push-mark)
    (if backward? (paredit-backward) (paredit-forward))

    (let ((result (vemv/selected-region with-properties?)))
      (pop-mark)
      (if backward? (paredit-forward) (paredit-backward))
      result)))

(defun vemv/duplicate (&optional backward?)
  "Copies the current line (or sexpr, if point is at the beggining of one, or selection, if the region is active),
inserting it at a new line."
  (interactive)

  (if (region-active-p)

      (let ((content (vemv/selected-region)))
        (dotimes (i (- (region-end) (point)))
          (forward-char))
        (insert content))

    (back-to-indentation)

    (if (and
         (vemv/in-a-lisp-mode?)
         (some (lambda (char)
                 (equal char (vemv/current-char-at-point)))
               '("(" "[" "{" "<" "\"")))
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

(defun vemv/ensure-no-double-blank-newlines ()
  (while (and (string-equal "\n" (vemv/current-line-contents))
              (string-equal "\n" (vemv/current-char-at-point -1))
              (string-equal "\n" (vemv/current-char-at-point -2)))
    (delete-backward-char 1))
  (while (and (string-equal "\n" (vemv/current-line-contents))
              (string-equal "\n" (vemv/current-char-at-point 1)))
    (delete-forward-char 1)))

(defun vemv/kill (&optional backward? _ skip-kill-whitespace?)
  "Deletes the next sexpr (or previous, if BACKWARD?).

   Unlike paredit-kill, this function will only grab one sexpr (and no more, if they are contigous),
   and it doesn't alter the kill-ring.

   Sexprs can only be killed having the cursor placed at the beginning or end of them, or at whitespace characters between sexprs
   But one cannot kill a sexpr (particularly symbols) when the cursor is placed in the middle: e.g. cursor `|` in `forbid|dden`"
  (interactive)
  (setq vemv.kill/did-kill-whitespace nil)
  (unless skip-kill-whitespace?
    (while (and (or (equal " " (vemv/current-char-at-point))
                    (equal "\n" (vemv/current-char-at-point))
                    (if backward?
                        (or (equal " " (vemv/current-char-at-point -1))
                            (equal "\n" (vemv/current-char-at-point -1)))
                      nil))
                (if backward?
                    (or (equal " " (vemv/current-char-at-point -1))
                        (equal "\n" (vemv/current-char-at-point -1)))
                  t))
      (setq vemv.kill/did-kill-whitespace t)
      (if backward?
          (delete-backward-char 1)
        (delete-forward-char 1))))
  ;; For case `foo| bar`, where one space should be deleted, resulting in `foobar`, in which case we should delete `bar`
  (when (or (and vemv.kill/did-kill-whitespace
                 (not (member (string-to-char (vemv/current-char-at-point))
                              (string-to-list "()[]{}#@`~ \n"))))
            (eq (point)
                (save-excursion
                  (if backward?
                      (progn
                        (paredit-backward)
                        (paredit-forward))
                    (progn
                      (paredit-forward)
                      (paredit-backward)))
                  (point))))
    (ignore-errors
      (push-mark)
      (if backward? (paredit-backward) (paredit-forward))
      (let ((result (vemv/selected-region)))
        (delete-region (mark) (point))
        (unless skip-kill-whitespace?
          (while (and
                  (equal " " (vemv/current-char-at-point))
                  (not (equal "\n" (vemv/current-char-at-point))))
            (paredit-forward-delete)))
        (unless skip-kill-whitespace?
          (ignore-errors
            (vemv/ensure-no-double-blank-newlines)))
        result))))

(defun vemv/delete-backward (&optional cut?)
  "Performs a paredit-backward-delete unless the region is active, in which case the selection gets unconditionally removed.

   The removed value will be pushed to the kill-ring only on non-nil values of CUT?.

   Unconditionally removing code may yield semantically wrong results, i.e. leaving sexprs unmatched.
   I personally like this tradeoff - use with caution!"
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

(defun vemv/backward-up-list ()
  "Like backward-up-list, but doesn't complain if invoked at ( itself"
  (while (not (some (lambda (char)
                      (equal char (vemv/current-char-at-point)))
                    '("(" "[" "{")))
    (beginning-of-sexp)))

(defun vemv/dumb-indent ()
  (interactive)
  (vemv/paredit-safely
   (vemv/backward-up-list)
   (paredit-wrap-sexp)
   (paredit-splice-sexp-killing-backward))
  (when (eq vemv/comment-indent-function 'vemv/normal-indentation)
    (let* ((initial-line (vemv/current-line-number))
           (final-line (save-excursion
                         (vemv/paredit-safely
                          (paredit-forward)
                          (vemv/current-line-number)))))
      (goto-line initial-line)
      (while (not (eq (vemv/current-line-number)
                      final-line))
        (progn
          (beginning-of-line)
          (call-interactively 'indent-for-tab-command)
          (next-line)))
      (goto-line initial-line)
      (beginning-of-line-text))))

(defun vemv/indent ()
  "Indents the next sexpr."
  (interactive)
  (vemv/dumb-indent))

(defun vemv/home ()
  "Moves the point to leftmost non-empty character in the current line."
  (interactive)
  (move-beginning-of-line 1)
  (if (not (equal last-command 'vemv/home))
      (while (some (lambda (char)
                     (equal char (vemv/current-char-at-point)))
                   '(" " "\t"))
        (forward-char))))

(defun vemv/end () ;; XXX doesn't honor region
  "Moves the point to rightmost non-empty character in the current line.

   Comments get ignored, this is, point will only move as long as its position still belongs to the code -
   unless this command has been fired for the second time."
  (interactive)
  (if (equal last-command 'vemv/end)
      (call-interactively 'move-end-of-line)
    (let* ((line (vemv/current-line-contents))
           (rev (vemv/reverse line))
           (line_length (length line))
           (movement (or 0 ;; temporarily disabled - uses recur which I discarded
                         (recur-let ((result 0))
                                    (if (some (lambda (char)
                                                (equal char (substring line result (inc result))))
                                              '(";" "\n"))
                                        result
                                      (recur (inc result)))))))
      (move-beginning-of-line 1)
      (forward-char movement)
      ;; there may exist empty space between code and comment:
      (if (pos? movement)
          (while (not (some (lambda (char)
                              (equal char (vemv/current-char-at-point)))
                            '(" ")))
            (backward-char)))
      (comm backward-char (recur-let ((result 0))
                                     (if (or
                                          (equal result line_length)
                                          (equal " " (substring rev result (inc result))))
                                         result
                                       (recur (inc result))))))))

(defun vemv/copy-selection-or-next-sexpr ()
  (let ((content (if (region-active-p)
                     (vemv/selected-region)
                   (vemv/sexpr-content))))
    (when (region-active-p)
      (call-interactively 'cua-set-mark))
    (vemv/bounded-list/insert-at-head! content vemv/kill-list vemv/kill-list-bound)
    (simpleclip-set-contents content)))

(defun vemv/copy-sexpr-content-backward ()
  (interactive)
  (kill-new (vemv/sexpr-content :backward)))

(defun vemv/kill-backward ()
  (interactive)
  (vemv/kill :backward))

(defun vemv/kill-backward-copying-content ()
  (interactive)
  (kill-new (vemv/kill :backward)))

(defun vemv/onelineize ()
  "Turns the current sexpr into a oneliner"
  (interactive)
  (when (member (vemv/current-char-at-point) `("(" "[" "{"))
    (let ((replacement (replace-regexp-in-string "[\s|\n]+" " " (vemv/sexpr-content))))
      (vemv/kill nil t t)
      (insert (concat replacement " "))
      (when (string-equal " " (vemv/char-at-left))
        (paredit-backward-delete))
      (call-interactively 'paredit-backward))))

(defun vemv/normal-indentation ()
  "https://stackoverflow.com/a/14196835/569050"
  (let ((c (current-buffer))
        (m major-mode)
        (p (point)))
    (if (and (looking-at "\\s<\\s<\\(\\s<\\)?")
             (or (match-end 1) (/= (current-column) (current-indentation))))
        0
      (let ((curr (with-temp-buffer
                    (insert-buffer-substring c)
                    (when (vemv/in-a-clojure-mode? m)
                      (clojure-mode))
                    (goto-char p)
                    (end-of-line)
                    (insert "\n")
                    (call-interactively 'indent-for-tab-command)
                    (current-indentation))))
        (when (or (/= (current-column) curr)
                  (and (> comment-add 0) (looking-at "\\s<\\(\\S<\\|\\'\\)")))
          curr)))))

(defun vemv/semicolon ()
  (interactive)
  (vemv/paredit-almost-safely
   (if (or (equal (vemv/current-char-at-point) ";")
           (paredit-in-string-p)
           (paredit-in-comment-p))
       (insert ";")
     (let* ((what (if (eq vemv/comment-indent-function 'vemv/normal-indentation)
                      ";"
                    ";;"))
            (_ (when (s-blank-str? (vemv/current-line-contents))
                 (call-interactively 'indent-for-tab-command)))
            (whitespace-chars (string-to-list " \n"))
            (maybe-newline (when (-any? (lambda (x)
                                          (not (member x whitespace-chars)))
                                        (string-to-list (vemv/chars-at-right)))
                             "\n"))
            (maybe-prefix (if (or (vemv/at-beginning-of-line-p)
                                  (string-equal " " (vemv/char-at-left)))
                              ""
                            " ")))
       (insert (concat maybe-prefix what " " maybe-newline))
       (when maybe-newline
         (beginning-of-line)
         (call-interactively 'indent-for-tab-command)
         (previous-line)
         (beginning-of-line)
         (call-interactively 'indent-for-tab-command)
         (end-of-line))))))

(setq vemv/thread-message
      "Press 1 to thread-first,\n      2 to fully thread-first,\n      3 to thread-last, or\n      4 to fully thread-last:\n\n")

(setq vemv.offer-onelineize/choices (string-to-list "1234"))

(defun vemv/offer-onelineize ()
  (unless (eq (read-char-choice "Press any of (1, 2, 3, 4) to `vemv/onelineize` the result, or RET for leaving it as-is:"
                                (cons 13 vemv.offer-onelineize/choices))
              13)
    (vemv/onelineize))
  ;; read-char-choice leaves some hanging output:
  (message ""))

(defun vemv/thread ()
  (interactive)
  (let* ((choice (read-char-choice vemv/thread-message vemv.offer-onelineize/choices)))
    (case choice
      (?1 (clojure-thread-first-all t))
      (?2 (clojure-thread-first-all nil))
      (?3 (clojure-thread-last-all t))
      (?4 (clojure-thread-last-all nil)))
    (vemv/offer-onelineize)))

(defun vemv/thread-first-all--but-last ()
  (interactive)
  (clojure-thread-first-all t)
  (vemv/offer-onelineize))

(defun vemv/thread-first-all--and-last ()
  (interactive)
  (clojure-thread-first-all nil)
  (vemv/offer-onelineize))

(defun vemv/thread-last-all--but-last ()
  (interactive)
  (clojure-thread-last-all t)
  (vemv/offer-onelineize))

(defun vemv/thread-last-all--and-last ()
  (interactive)
  (clojure-thread-last-all nil)
  (vemv/offer-onelineize))
