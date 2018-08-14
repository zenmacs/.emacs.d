;; -*- lexical-binding: t; -*-

(setq lexical-binding t)

(provide 'vemv.hooks)

(when (not vemv-cleaning-namespaces)
  (add-hook 'clojure-mode-hook 'hs-minor-mode))

(add-hook 'ruby-mode-hook (argless (ruby-end-mode)
                                   (vemv/set-keys-for-scope ruby-mode-map vemv/ruby-key-bindings)
                                   (define-key ruby-mode-map [tab] 'vemv/tab)))

(add-hook 'emacs-lisp-mode-hook
          (argless (setq-local mode-line-format tabbed-line-format)))

(add-hook 'cider-repl-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(add-hook 'ielm-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(add-hook 'shell-mode-hook
          (argless (setq-local mode-line-format vemv/pe/mode-line-format)))

(dolist (mode (list 'emacs-lisp-mode-hook 'ruby-mode-hook 'clojure-mode-hook
                    'js-mode-hook 'css-mode-hook 'html-mode-hook 'haml-mode-hook))
  (add-hook mode (argless (call-interactively 'text-scale-increase))))

(advice-add 'pe/show-buffer :after 'vemv/after-file-open)
(advice-add 'vemv/fiplr :after 'vemv/after-file-open)
(advice-add 'vemv/open :after 'vemv/after-file-open)
(advice-add 'vemv/next-file-buffer :after 'vemv/after-file-open)
(advice-add 'vemv/previous-file-buffer :after 'vemv/after-file-open)
(advice-add 'vemv/close-this-buffer :after 'vemv/after-file-open)
(advice-add 'helm-ag--action-find-file :after 'vemv/after-file-open)
(advice-add 'cider-new-error-buffer :after (lambda (&rest _)
                                             (cider-interactive-eval "(try (clojure.core/prn clojure.core/*e)
                                                                              (catch java.lang.Throwable e))")
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

(add-hook 'clojure-mode-hook
          (argless (enable-paredit-mode)
                   (paren-face-mode 1)
                   (global-set-key (kbd "C-r") 'vemv/test-this-ns) ;; must be defined there. TODO: define all clojure bindings here
                   (setq-local mode-line-format tabbed-line-format)))

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(add-hook 'ielm-mode-hook 'enable-paredit-mode)

(add-hook 'haml-mode-hook (argless
                           (require 'highlight-indent-guides)
                           (setq highlight-indent-guides-auto-enabled nil)
                           (set-face-foreground 'highlight-indent-guides-character-face vemv-default-foreground-color-much-darker)
                           (set-face-background 'highlight-indent-guides-top-character-face vemv-colors/purple)
                           (set-face-foreground 'highlight-indent-guides-top-character-face vemv-colors/purple)
                           (setq global-hl-line-mode nil)
                           (highlight-indent-guides-mode)))

(add-hook 'haml-mode-hook (argless
                           (vemv/set-keys-for-scope haml-mode-map vemv/ruby-key-bindings)
                           (define-key haml-mode-map [tab] 'vemv/tab)))

(add-hook 'js-mode-hook (argless
                         (vemv/set-keys-for-scope js-mode-map vemv/ruby-key-bindings)
                         (define-key js-mode-map [tab] 'vemv/tab)))

(add-hook 'sh-mode-hook (argless
                         (vemv/set-keys-for-scope sh-mode-map vemv/ruby-key-bindings)
                         (define-key sh-mode-map [tab] 'vemv/tab)))

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

;; for when one opens a file via the terminal
;; disabled, seems to mess up tabs
;; (add-hook 'buffer-list-update-hook 'vemv/clean-chosen-file-buffer-order)

;; https://stackoverflow.com/questions/14243583/semantic-movement-across-a-line
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

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode clojure-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(advice-add 'vemv/jump-to-clojure-definition :after 'vemv/clean-chosen-file-buffer-order)
(advice-add 'xref-pop-marker-stack :after 'vemv/clean-chosen-file-buffer-order)

(setq vemv/clicking-left-click nil)

(defun vemv/pe/left-click (f &rest args)
  (let ((vemv/clicking-left-click t))
    (apply f args)))

(defun vemv/pe/middle-click (x)
  (when (not vemv/clicking-left-click)
    (kill-buffer (window-buffer vemv/main_window)))
  t)

;; Thanks to these, by clicking middle click one closes the current file before opening the chosen one.
;; On macOS: fn + click.
(comm ;; disabled - stopped working for some reason
 (advice-add 'pe/left-click ':around 'vemv/pe/left-click)
 (advice-add 'pe/middle-click ':before 'vemv/pe/middle-click))

;; https://github.com/DarthFennec/highlight-indent-guides/issues/44#issuecomment-411486188
(defadvice highlight-indent-guides--update-line-cache
    (around my-update-line-cache activate)
  (let ((higp 'highlight-indent-guides-prop) pos indent)
    (save-excursion
      (beginning-of-line)
      (while (and (not (eobp))
                  (or (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))
                      (looking-at "[[:space:]]*$")))
        (forward-line))
      (setq pos (point) indent (current-indentation))
      (forward-line)
      (while (and (not (eobp))
                  (or (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))
                      (looking-at "[[:space:]]*$")))
        (forward-line))
      (unless (< indent (current-indentation)) (goto-char pos))
      (back-to-indentation)
      (setq ad-return-value
            (unless (bolp) (nth 5 (get-text-property (1- (point)) higp)))))))

(advice-add 'helm-ag--edit :after 'vemv/ag-replace)

(advice-add 'cider-test-run-test :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-run-ns-tests :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-run-project-tests :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-rerun-failed-tests :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-show-report :around 'vemv/apply-tests-verbosely)
(advice-add 'cider-test-execute :around 'vemv/apply-tests-verbosely)

(defun hack-local-variables-confirm (f &rest args)
  "Disables annoying dialog 'The local variables list in :x contains values that may not be safe"
  t)

(when pe/cache-enabled
  (add-hook 'kill-emacs-hook 'pe/cache-clear))
