;;; -*- lexical-binding: t -*-

(setq lexical-binding t)

(require 'vemv.lang)
(require 'vemv.project)
(require 'vemv.workspace)
(require 'vemv.data)
(require 'vemv.data.bindings)
(require 'vemv.theme)
(require 'vemv.setqs)
(provide 'vemv.init)

(show-paren-mode 1)
(recentf-mode 1)
(ido-mode 1)
(blink-cursor-mode -1)
(tooltip-mode -1)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(global-subword-mode)

(global-company-mode)

(menu-bar-mode)
(global-auto-revert-mode t) ;; refresh buffers on external changes to the underlying files

(global-hl-line-mode t)
(global-whitespace-mode)

;; initialized after customizing cua-remap-control-v
(cua-mode 1)

(when (not vemv-cleaning-namespaces)
  (add-hook 'clojure-mode-hook 'hs-minor-mode))

(add-hook 'ruby-mode-hook (argless (smartparens-mode)))

(add-hook 'emacs-lisp-mode-hook
          (argless (setq-local mode-line-format tabbed-line-format)))

(add-hook 'cider-repl-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(add-hook 'ielm-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(add-hook 'shell-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(dolist (mode (list 'emacs-lisp-mode-hook 'ruby-mode-hook 'clojure-mode-hook 'js-mode-hook 'css-mode-hook 'html-mode-hook))
  (add-hook mode (argless (call-interactively 'text-scale-increase))))

(add-hook 'clojure-mode-hook
          (argless (enable-paredit-mode)
                   (clj-refactor-mode 1)
                   (paren-face-mode 1)
                   (undo-tree-mode)
                   (cljr-add-keybindings-with-prefix "C-0")
                   (global-set-key (kbd "C-r") 'vemv/test-this-ns) ;; must be defined there. TODO: define all clojure bindings here
                   (setq-local mode-line-format tabbed-line-format)))

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(add-hook 'ielm-mode-hook 'enable-paredit-mode)

(add-hook 'cider-connected-hook
          (argless
           (delay (argless
                   (setq vemv-cider-connecting nil)
                   (setq vemv-cider-connected t)
                   (vemv/show-clj-or-cljs-repl)
                   (when (not vemv-cleaning-namespaces)
                     (vemv/advice-nrepl)))
                  2)))
(add-hook 'cider-repl-mode-hook #'paredit-mode)

;; Important - remove keybindings before (vemv/initial-layout) so M-x cannot interrupt

(dolist (mode (list paredit-mode-map comint-mode-map undo-tree-map cider-mode-map))
  (mapc (lambda (arg)
          (define-key mode (vemv/keyboard-macro arg) nil))
        vemv/exhaustive-list-of-bindings-to-remove))

(dolist (key vemv/key-bindings-to-remove)
  (global-unset-key key))

(dolist (key vemv/key-bindings-to-dummy)
  (global-set-key key (argless)))

(dolist (binding (vemv/partition 3 vemv/local-key-bindings))
  (define-key
    (car binding)
    (let ((k (second binding)))
      (if (stringp k)
          (read-kbd-macro k)
          k))
    (third binding)))

(vemv/initial-layout)

(shell-command-to-string "touch ~/.emacs.d/custom.el")
(load custom-file)

(delay 'vemv/clojure-init 1)

(delay (argless (if (window-system)
                    (set-face-attribute 'default nil :font vemv-font)))
       1)

(defadvice back-to-indentation (around back-to-back)
  (if (eq last-command this-command)
      (progn
        (if back-to-indentation-state
            ad-do-it
            (beginning-of-line)
            (send! back-to-indentation-state 'not)))
      (progn
        (setq back-to-indentation-state nil)
        ad-do-it)))

(ad-activate 'back-to-indentation)

(defun vemv/make-frame ()
  (make-frame `((width . ,(frame-width)) (height . ,(frame-height)))))

(defvar vemv/help-frame nil)

(defmacro vemv/get-help-frame ()
  `(if (and vemv/help-frame (terminal-live-p vemv/help-frame))
       vemv/help-frame
       (let ((frame (vemv/make-frame)))
         (select-frame frame)
         (vemv/maximize)
         (setq vemv/help-frame frame))))

(defun vemv/display-completion (buffer)
  (vemv/safe-select-window vemv/main_window)
  (set-window-buffer vemv/main_window buffer))

(defun undo (&rest args)
  (interactive)
  (apply 'undo-tree-undo args))

(global-set-key [kp-delete] 'delete-char) ;; OSX

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode clojure-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(delay
 (argless (setq vemv/project-explorer-initialized t))
 12)

(delay
 ;; every 5 seconds. in practice, not so often b/c `vemv/refreshing-caches` (timestamp lock)
 (argless (run-with-timer 0 5 'vemv/refresh-file-caches))
 60)

(setq company-dabbrev-char-regexp "\\sw\\|-")

;; monkeypatch for https://github.com/clojure-emacs/cider/issues/2102
(defun cider--format-buffer (formatter)
  "Format the contents of the current buffer.

Uses FORMATTER, a function of one argument, to convert the string contents
of the buffer into a formatted string."
  (let* ((original (substring-no-properties (buffer-string)))
         (formatted (funcall formatter original)))
    (if (or (not formatted) (equal original formatted))
        (when (not formatted) (vemv/echo "Buffer has broken syntax, cannot format"))
        (erase-buffer)
        (insert formatted))))

(advice-add 'helm-ag--edit :after 'vemv/ag-replace)

(advice-add 'cider-test-run-test :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-run-ns-tests :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-run-project-tests :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-rerun-failed-tests :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-show-report :around 'vemv/apply-tests-verbosely)

;; this is a defmacro so `M-x describe-key` doesn't show a giantic hash, freezing emacs
(defmacro vemv/set-keys-for-scope (scope source)
  `(maphash (lambda (key _)
             (let* ((keyboard-macro (vemv/keyboard-macro key)))
               (if (eq ,scope :global)
                   (global-set-key keyboard-macro
                                   (argless (call-interactively (gethash key ,source))))
                   (define-key ,scope
                     keyboard-macro
                     (argless (call-interactively (gethash key ,source)))))))
           ,source))

(vemv/set-keys-for-scope :global vemv/global-key-bindings)

(vemv/set-keys-for-scope clojure-mode-map vemv/clojure-key-bindings)

(vemv/set-keys-for-scope ruby-mode-map vemv/ruby-key-bindings)

(require 'vemv.desktop)
