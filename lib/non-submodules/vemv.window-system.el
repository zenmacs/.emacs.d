;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.window-system)

(defun vemv/initial-layout ()
  
  (when (window-system)
      (vemv/maximize))

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

  (setq vemv/repl-window (selected-window))

  (delay (argless (vemv/safe-select-window vemv/repl-window)
                  (ielm)
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
      (eq (selected-window) vemv/repl-window)
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
                                             (not (seq-contains (list vemv/repl-window vemv/project-explorer-window vemv/main_window)
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
            (-clone (gethash vemv/current-project vemv/chosen-file-buffer-order)))
    (vemv/next-file-buffer)))

(defun vemv/maximize ()
  "Maximize the current frame. Presumes an X-window environment."
  (toggle-frame-maximized))

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

(defun vemv/new-frame ()
  (interactive)
  ;; in order to kill a frame, use the window system's standard exit (e.g. Alt-F4) command. The other frames won't close
  (make-frame `((width . ,(frame-width)) (height . ,(frame-height)))))

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
