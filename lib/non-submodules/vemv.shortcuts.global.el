(require 'vemv.lang)
(require 'vemv.shortcuts.global.base)
(provide 'vemv.shortcuts.global)

(setq vemv/shortcuts/global/backspace               'vemv/backspace
      vemv/shortcuts/global/down                    'next-line
      vemv/shortcuts/global/end                     'vemv/end-of-line-or-code
      vemv/shortcuts/global/RET                     'newline-and-indent
      vemv/shortcuts/global/S-backspace             'vemv/force-backspace
      vemv/shortcuts/global/S-f7                    'vemv/previous-workspace
      vemv/shortcuts/global/S-f9                    'vemv/next-workspace
      vemv/shortcuts/global/f2                      (argless
                                                     (vemv/safe-select-window vemv/main_window)
                                                     (switch-to-buffer "*scratch*"))
      vemv/shortcuts/global/f3                      'split-window-below
      vemv/shortcuts/global/f4                      'split-window-right
      vemv/shortcuts/global/f5                      (argless
                                                     (vemv/safe-select-window vemv/repl-window)
                                                     (switch-to-buffer "*shell-1*")
                                                     (vemv/send :shell nil (concat vemv.project/cd-command vemv/project-root-dir))
                                                     (comint-clear-buffer))
      vemv/shortcuts/global/f6                      (argless
                                                     (vemv/safe-select-window vemv/main_window)
                                                     (let* ((what (vemv/send :shell nil (or (-some->> (file-name-directory (buffer-file-name))
                                                                                                      (concat vemv.project/cd-command))
                                                                                            (concat vemv.project/cd-command vemv/project-root-dir)))))
                                                       (vemv/safe-select-window vemv/repl-window)
                                                       (switch-to-buffer "*shell-1*")
                                                       (vemv/send :shell nil what)
                                                       (comint-clear-buffer)))
      vemv/shortcuts/global/f7                      'vemv/previous-project
      vemv/shortcuts/global/f8                      'vemv/after-file-open
      vemv/shortcuts/global/f9                      'vemv/next-project
      vemv/shortcuts/global/home                    'back-to-indentation
      vemv/shortcuts/global/left                    'left-char
      vemv/shortcuts/global/next                    'cua-scroll-up
      vemv/shortcuts/global/primary-1               'vemv/onelineize
      vemv/shortcuts/global/primary-2               (argless
                                                     (call-interactively 'goto-line))
      ;; NOTE: don't wrap it in vemv/safe-paredit-command, it already does so internally:
      vemv/shortcuts/global/primary-3               'vemv/indent
      vemv/shortcuts/global/primary-6               'vemv/emacs-reload
      vemv/shortcuts/global/primary-7               'vemv/toggle-between-implementation-and-test
      vemv/shortcuts/global/primary-4               'vemv/thread
      vemv/shortcuts/global/primary-5               (argless
                                                     (let* ((clojure-align-binding-forms (apply 'list "speced/let" '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop" "doseq" "for" "with-open" "with-local-vars" "with-redefs"))))
                                                       (call-interactively 'clojure-align)))
      vemv/shortcuts/global/primary-8               'vemv/toggle-verbosity
      vemv/shortcuts/global/primary-9               (argless
                                                     (call-interactively 'clojure-align))
      vemv/shortcuts/global/primary-S-a             'vemv/git-add-A
      vemv/shortcuts/global/primary-S-d             (argless
                                                     (require 'vemv.helm)
                                                     (vemv/helm-search-and-replace-with-previews))
      vemv/shortcuts/global/primary-S-f             (argless
                                                     (require 'vemv.helm)
                                                     (vemv/helm-search-and-replace))
      vemv/shortcuts/global/primary-S-n             (argless
                                                     (let ((f (vemv/new-frame)))
                                                       (select-frame f)
                                                       (switch-to-buffer "*Messages*")))
      vemv/shortcuts/global/primary-S-s             'save-buffer
      vemv/shortcuts/global/primary-S-w             'vemv/close-all-file-buffers
      vemv/shortcuts/global/primary-S-z             (argless
                                                     (require 'vemv.undo)
                                                     (call-interactively 'undo-tree-redo))
      vemv/shortcuts/global/primary-SPC             'cua-set-mark
      vemv/shortcuts/global/primary-a               'vemv/copy-selection-or-next-sexpr
      vemv/shortcuts/global/primary-b               'vemv/duplicate
      vemv/shortcuts/global/primary-bar             (argless
                                                     (insert "\n"))
      vemv/shortcuts/global/primary-backspace       'vemv/delete-this-line
      vemv/shortcuts/global/primary-backtick        (argless
                                                     (call-interactively 'other-frame))
      vemv/shortcuts/global/primary-bang            (argless
                                                     (revert-buffer t t t))
      vemv/shortcuts/global/primary-dash            'vemv/echo-clojure-source
      vemv/shortcuts/global/primary-down            'forward-paragraph
      vemv/shortcuts/global/primary-dot             (argless

                                                     (defun magit-diff-visit-file (file &optional other-window)
                                                       (interactive (list (magit-diff--file-at-point t t) current-prefix-arg))
                                                       (magit-diff-visit-file--internal file
                                                                                        nil
                                                                                        (lambda (buffer)
                                                                                          (set-window-buffer vemv/main_window buffer)
                                                                                          (select-frame-set-input-focus vemv/main_frame))))

                                                     (let* ((default-directory (or (when buffer-file-name
                                                                                     (file-name-directory buffer-file-name))
                                                                                   vemv/project-root-dir))
                                                            (found (car (seq-filter (lambda (b)
                                                                                      (with-current-buffer b
                                                                                        (and (get-buffer-window b t)
                                                                                             (equal 'magit-status-mode major-mode))))
                                                                                    (buffer-list))))
                                                            (focused? (equal (selected-frame)
                                                                             (window-frame (get-buffer-window found t)))))
                                                       (if (and found focused?)
                                                           (select-frame-set-input-focus (window-frame vemv/main_window))
                                                         (if (and found (not focused?))
                                                             (progn
                                                               (select-frame-set-input-focus (window-frame (get-buffer-window found t)))
                                                               (with-current-buffer found
                                                                 (call-interactively 'magit-refresh)))
                                                           (progn
                                                             (condition-case nil
                                                                 (vemv/save-all-buffers-for-this-project t t)
                                                               (error nil))
                                                             (call-interactively 'magit-status-here))))))
      vemv/shortcuts/global/primary-e               'vemv/send
      vemv/shortcuts/global/primary-equal           'mark-whole-buffer
      vemv/shortcuts/global/primary-f               'vemv/search-in-this-buffer
      vemv/shortcuts/global/primary-j               (argless
                                                     (if (vemv/in-a-clojure-mode?)
                                                         (vemv/clojure-init-or-send-sexpr)
                                                       (if (eq :ruby vemv/project-type)
                                                           (unless (or vemv-robe-connecting vemv-robe-connected)
                                                             (let ((default-dir vemv/project-root-dir)
                                                                   (inf-ruby-console-environment "development"))
                                                               (setq vemv-robe-connecting t)
                                                               (inf-ruby-console-auto))))))
      vemv/shortcuts/global/primary-k               'vemv/kill
      vemv/shortcuts/global/primary-left            (vemv/safe-paredit-command 'paredit-forward-barf-sexp)
      vemv/shortcuts/global/primary-n               'vemv/new-frame
      vemv/shortcuts/global/primary-o               'vemv/open-at-pwd
      vemv/shortcuts/global/primary-p               'vemv/repeat-last-search-in-this-buffer
      vemv/shortcuts/global/primary-q               'save-buffers-kill-terminal
      vemv/shortcuts/global/primary-r               'vemv/test-this-ns
      vemv/shortcuts/global/primary-right           (vemv/safe-paredit-command 'paredit-forward-slurp-sexp)
      vemv/shortcuts/global/primary-s               'vemv/save
      vemv/shortcuts/global/primary-semicolon       'toggle-truncate-lines
      vemv/shortcuts/global/primary-t               'vemv/fiplr
      vemv/shortcuts/global/primary-u               'cljr-add-missing-libspec
      vemv/shortcuts/global/primary-up              'backward-paragraph
      vemv/shortcuts/global/primary-v               'vemv/paste-from-clipboard
      vemv/shortcuts/global/primary-w               'vemv/close-this
      vemv/shortcuts/global/primary-z               (argless
                                                     (require 'vemv.undo)
                                                     (call-interactively 'undo-tree-undo))
      vemv/shortcuts/global/primary-secondary-8     (argless (setq vemv/input-enabled t))
      vemv/shortcuts/global/primary-secondary-c     (argless
                                                     (require 'cider-ns)
                                                     (vemv/before-refresh nil)
                                                     (cider-ns-refresh 'clear-and-inhibit))
      vemv/shortcuts/global/primary-secondary-d     'vemv/delete-file-and-buffer
      vemv/shortcuts/global/primary-secondary-w     (argless
                                                     (replying-yes
                                                      (vemv/close-this)))
      vemv/shortcuts/global/primary-secondary-f     (argless
                                                     (require 'vemv.helm)
                                                     (vemv/helm-search-and-replace-from-this-directory))
      vemv/shortcuts/global/primary-secondary-q     (argless
                                                     (replying-yes
                                                      (kill-emacs)))
      vemv/shortcuts/global/prior                   'cua-scroll-down
      vemv/shortcuts/global/right                   'right-char
      vemv/shortcuts/global/secondary-7             'vemv/toggle-between-domain-and-web
      vemv/shortcuts/global/secondary-S-k           'vemv/kill-backward-copying-content
      vemv/shortcuts/global/secondary-a             'vemv/copy-sexpr-content-backward
      vemv/shortcuts/global/secondary-backspace     (vemv/safe-paredit-command 'paredit-backward-kill-word)
      vemv/shortcuts/global/secondary-backtick      (argless
                                                     (if (equal major-mode 'cider-inspector-mode)
                                                         (cider-inspector-pop)
                                                       (xref-pop-marker-stack)))
      vemv/shortcuts/global/secondary-bar           'vemv/toggle-all
      vemv/shortcuts/global/secondary-colon         (argless (call-interactively 'eval-expression))
      vemv/shortcuts/global/secondary-e             (argless (vemv/send nil t))
      vemv/shortcuts/global/secondary-f             'cider-test-toggle-fail-fast
      vemv/shortcuts/global/secondary-k             'vemv/kill-backward
      vemv/shortcuts/global/secondary-left          'backward-word
      vemv/shortcuts/global/secondary-left-bracket  (vemv/safe-paredit-command 'paredit-backward)
      vemv/shortcuts/global/secondary-left-curly    (vemv/safe-paredit-command 'paredit-wrap-square)
      vemv/shortcuts/global/secondary-left-parens   (vemv/safe-paredit-command 'paredit-wrap-sexp)
      vemv/shortcuts/global/secondary-next          'previous-buffer
      vemv/shortcuts/global/secondary-o             'vemv/open-at-project-root
      vemv/shortcuts/global/secondary-prior         'next-buffer
      vemv/shortcuts/global/secondary-RET           'vemv/toggle-ns-hiding
      vemv/shortcuts/global/secondary-right         'forward-word
      vemv/shortcuts/global/secondary-right-bracket (vemv/safe-paredit-command 'paredit-forward)
      vemv/shortcuts/global/secondary-right-curly   (vemv/safe-paredit-command 'paredit-wrap-curly)
      vemv/shortcuts/global/secondary-s             'vemv/save-other-buffers-for-this-project
      vemv/shortcuts/global/secondary-semicolon     (argless
                                                     (insert " ;; "))
      vemv/shortcuts/global/secondary-single-quote 'vemv/focus
      vemv/shortcuts/global/secondary-t             'vemv/open-file-via-fiplr-then-close-previous-buffer
      vemv/shortcuts/global/secondary-up            (vemv/safe-paredit-command
                                                     (argless
                                                      (when (or (paredit-in-string-p)
                                                                (ignore-errors ;; avoid running the command at the top-level.
                                                                  (save-excursion
                                                                    (backward-up-list))
                                                                  t))
                                                        (paredit-splice-sexp-killing-backward))))
      vemv/shortcuts/global/secondary-x             'vemv/smex
      vemv/shortcuts/global/secondary-w             'vemv/close-all-other-file-buffers
      vemv/shortcuts/global/tertiary-1              (vemv/safe-paredit-command 'vemv/thread-first-all--but-last)
      vemv/shortcuts/global/tertiary-2              (vemv/safe-paredit-command 'vemv/thread-first-all--and-last)
      vemv/shortcuts/global/tertiary-3              (vemv/safe-paredit-command 'vemv/thread-last-all--but-last)
      vemv/shortcuts/global/tertiary-4              (vemv/safe-paredit-command 'vemv/thread-last-all--and-last)
      vemv/shortcuts/global/tertiary-5              (vemv/safe-paredit-command 'clojure-unwind-all)
      vemv/shortcuts/global/tertiary-7              'vemv/toggle-between-api-and-implementation
      vemv/shortcuts/global/tertiary-9              (vemv/safe-paredit-command 'paredit-backward-up)
      ;; NOTE: tertiary-0 will not work unless one unbinds it from macOS.
      vemv/shortcuts/global/tertiary-0              (vemv/safe-paredit-command 'paredit-forward-up)

      vemv/shortcuts/global/tertiary-SPC            'vemv/indent-region
      vemv/shortcuts/global/tertiary-backspace      'vemv/unindent-region
      vemv/shortcuts/global/tertiary-a              'vemv/copy-inserting-at-kill-list
      vemv/shortcuts/global/tertiary-backtick       'vemv/jump-to-clojure-definition
      vemv/shortcuts/global/tertiary-bar            (vemv/safe-paredit-command 'paredit-split-sexp)
      vemv/shortcuts/global/tertiary-down           'end-of-defun
      vemv/shortcuts/global/tertiary-e              (argless (vemv/send nil nil nil :no-return))
      vemv/shortcuts/global/tertiary-equal          'cider-load-buffer
      vemv/shortcuts/global/tertiary-end            'end-of-buffer
      vemv/shortcuts/global/tertiary-f              (argless
                                                     (vemv/save)
                                                     (require 'vemv.helm)
                                                     (vemv/with-helm-follow nil
                                                       (call-interactively 'helm-do-ag-this-file)))
      vemv/shortcuts/global/tertiary-home           'beginning-of-buffer
      vemv/shortcuts/global/tertiary-j              (argless
                                                     (if (vemv/in-a-clojure-mode?)
                                                         (cider-eval-sexp-at-point)
                                                       (vemv/send :emacs)))
      vemv/shortcuts/global/tertiary-k              'vemv/cut
      vemv/shortcuts/global/tertiary-left           'vemv/previous-file-buffer
      vemv/shortcuts/global/tertiary-left-bracket   'vemv/pull-next-sexpr
      vemv/shortcuts/global/tertiary-o              'vemv/open-project
      vemv/shortcuts/global/tertiary-r              'cider-test-run-test
      vemv/shortcuts/global/tertiary-right          'vemv/next-file-buffer
      vemv/shortcuts/global/tertiary-s              'vemv/save-all-buffers-for-this-project
      vemv/shortcuts/global/tertiary-slash          'zenmacs.clojure-interaction/show-java-decompilation-of-top-level-sexpr
      vemv/shortcuts/global/tertiary-t              (argless
                                                     (vemv/fiplr (lambda (filename)
                                                                   (find-file filename)
                                                                   (vemv/close-all-other-file-buffers))))
      vemv/shortcuts/global/tertiary-up             'beginning-of-defun
      vemv/shortcuts/global/tertiary-v              'vemv/paste-from-kill-list
      vemv/shortcuts/global/tertiary-w              'vemv/close-all-file-buffers
      vemv/shortcuts/global/tertiary-x              'vemv/dumb-cut
      vemv/shortcuts/global/up                      'previous-line)

;; other S-RET syntaxes don't work. TODO: abstract away this
(global-set-key [(shift return)] (argless
                                  (if (vemv/in-a-clojure-mode?)
                                      (vemv/clear-cider-repl-buffer)
                                    (if (eq major-mode 'messages-buffer-mode)
                                        (let ((inhibit-read-only t))
                                          (erase-buffer))
                                      (let* ((l? (vemv/in-a-lisp-mode?))
                                             (r? (and (not l?)
                                                      (member major-mode '(ruby-mode inf-ruby-mode))
                                                      (get-buffer "*rails*"))))
                                        (when r?
                                          (when-let* ((w (get-buffer-window "*rspec-compilation*")))
                                            (with-selected-window w
                                              (vemv/close-this))))
                                        (with-selected-window vemv/repl-window
                                          (if l?
                                              (vemv/safe-switch-to-buffer "*ielm*")
                                            (if r?
                                                (vemv/safe-switch-to-buffer "*rails*")
                                              (vemv/safe-switch-to-buffer "*shell-1*")))
                                          (comint-clear-buffer)))))))

;; same here. control-ret is interpreted as s-return rather than as tertiary-RET
(global-set-key [(s return)] (argless
                              (require 'cider-ns)
                              (vemv/before-refresh t)
                              (cider-ns-refresh)))

(global-set-key (kbd "C-M-<return>") (argless
                                      (comm
                                       (when (vemv/ciderable-p)
                                         (vemv/load-clojure-buffer nil vemv/clojure-lightweight-reload-command)))
                                      (require 'cider-ns)
                                      (vemv/before-refresh nil)
                                      (cider-ns-refresh 'inhibit-fns)))
