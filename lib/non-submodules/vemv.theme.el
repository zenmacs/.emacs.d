;; for debugging:
;;   list-faces-display
;;   m-x describe-theme
;;   (buffer-string)

(require 'vemv.project)
(provide 'vemv.theme)

(deftheme vemv "")

(setq vemv-colors/blue "#6da5ff")
(setq vemv-colors/light-blue "#BFD8FF")
(setq vemv-colors/keyword-green "#C1ED3D")
(setq vemv-colors/yellow "#FFE33B")
(setq vemv-colors/lighter-yellow "#ffe444")
(setq vemv-colors/dark-yellow "#877b34")
(setq vemv-colors/paren-grey-strong "#c0c0c0")
(setq vemv-colors/paren-grey-light "#e0e0e0")
(setq vemv-colors/pink "#D93273")
(setq vemv-colors/purple "#7A3555")
(setq vemv-colors/warning-pink "#aa1e44")

(unless vemv/terminal-emacs?
  (set-face-background 'comint-highlight-prompt "#4D575F")
  (set-face-foreground 'comint-highlight-prompt vemv-colors/paren-grey-light))

(defface font-lock-line-and-column-face
  `((t :foreground "#696969"))
  "."
  :group 'vemv)

(defface vemv-cider-connection-face
  `((t :foreground "#CFCFCF"))
  "."
  :group 'vemv)

(defface vemv/blue-face
  `((t :foreground ,vemv-colors/blue))
  "."
  :group 'vemv)

(setq vemv-default-autocomplete-popup-background-color "#A2A2A2")
(setq vemv-default-autocomplete-popup-background-color-lighter "#cccccc")
(setq vemv-default-autocomplete-popup-foreground-color "#333333")

(setq vemv-default-background-color "#4D575F")
(setq vemv-default-background-color-slightly-darker "#404950")
(setq vemv-default-foreground-color "#F5F5F5")
(setq vemv-default-foreground-color-much-darker "#a8a8a8")
(setq vemv-default-foreground-color-very-slightly-darker "#ededed")
(setq vemv-error-foreground-color vemv-default-foreground-color)

(set-variable 'cider-stacktrace-frames-background-color vemv-default-background-color)
(set-variable 'cider-test-items-background-color vemv-default-background-color)

(defface vemv-default-face
  `((t :foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))
  "."
  :group 'vemv)

(defface vemv-default-foreground-face-very-slightly-darker
  `((t :foreground ,vemv-default-foreground-color-very-slightly-darker))
  "."
  :group 'vemv)

(defface vemv-warning-face
  `((t :foreground ,vemv-colors/yellow :background ,vemv-default-background-color-slightly-darker))
  "."
  :group 'vemv)

(defface vemv-reverse-warning-face
  `((t :underline (:color ,vemv-colors/yellow :style wave)))
  "."
  :group 'vemv)

;; Faces aren't vars, so e.g. ruby-mode can break without these
(defvar vemv-default-foreground-face-very-slightly-darker 'vemv-default-foreground-face-very-slightly-darker)
(defvar vemv-default-face 'vemv-default-face)
(defvar font-lock-line-and-column-face 'font-lock-line-and-column-face)
(defvar vemv/blue-face 'vemv/blue-face)
(defvar clojure-global-constant-face 'clojure-global-constant-face)

(set-fringe-bitmap-face 'left-arrow 'vemv-cider-connection-face)
(set-fringe-bitmap-face 'left-curly-arrow 'vemv-cider-connection-face)
(set-fringe-bitmap-face 'right-arrow 'vemv-cider-connection-face)
(set-fringe-bitmap-face 'right-curly-arrow 'vemv-cider-connection-face)

(custom-theme-set-faces
 'vemv

 `(parenthesis ,(if (eq :ruby vemv/project-type)
                    `((t (:foreground ,vemv-colors/paren-grey-light)))
                  `((t (:foreground ,vemv-colors/paren-grey-strong)))))

 `(term-color-black      ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
 `(term-color-red        ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
 `(term-color-green      ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
 `(term-color-yellow     ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
 `(term-color-blue       ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
 `(term-color-magenta    ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
 `(term-color-cyan       ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
 `(term-color-white      ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
 `(term-default-fg-color ((t (:inherit term-color-white))))
 `(term-default-bg-color ((t (:inherit term-color-black))))

 `(grizzl-selection-face
   ((((class color) (background light))
     (:foreground ,vemv-colors/keyword-green))
    (((class color) (background dark))
     (:foreground ,vemv-colors/keyword-green))
    (t (:foreground ,vemv-colors/keyword-green))))

 `(default ((t (:background ,vemv-default-background-color :foreground ,vemv-default-foreground-color))))
 `(default ((t (:background ,vemv-default-background-color :foreground ,vemv-default-foreground-color))))
 `(cursor ((t (:foreground "#4D575F" :background "#CFCFCF"))))
 `(blue ((t (:foreground "blue"))))
 `(bold ((t (:bold t))))
 `(bold-italic ((t (:bold t :italc t))))
 `(border-glyph ((t (nil))))
 `(buffers-tab ((t (:foreground "#F8F8F8"))))

 `(whitespace-line ((t (:background ,vemv-colors/pink :foreground ,vemv-default-foreground-color))))

 `(compilation-info ((t (:background ,vemv-default-background-color :foreground ,vemv-default-foreground-color))))

 `(pe/directory-face ((t (:foreground ,vemv-colors/lighter-yellow :bold t))))
 `(pe/file-face ((t (:foreground ,vemv-colors/paren-grey-light :bold t))))

 `(cider-fringe-good-face ((t (:foreground ,vemv-default-foreground-color))))
 `(cider-stacktrace-error-message-face ((t (:foreground ,vemv-default-foreground-color-very-slightly-darker))))
 `(cider-error-highlight-face ((t (:foreground ,vemv-default-foreground-color))))
 `(cider-stacktrace-error-class-face ((t (:foreground ,vemv-error-foreground-color))))
 `(cider-repl-stderr-face ((t (:foreground ,vemv-error-foreground-color))))
 `(cider-repl-prompt-face ((t (:foreground ,vemv-colors/paren-grey-light :italic t))))
 `(cider-repl-stdout-face ((t (:foreground ,vemv-default-foreground-color-very-slightly-darker))))
 `(cider-stacktrace-error-class-face ((t (:foreground ,vemv-error-foreground-color))))
 `(cider-stacktrace-ns-face ((t (:foreground "white"))))
 `(cider-stacktrace-filter-active-face ((t (:foreground ,vemv-colors/keyword-green :underline t))))
 `(cider-stacktrace-filter-inactive-face ((t (:foreground ,vemv-colors/keyword-green))))
 `(cider-test-success-face
   ((t (:background ,vemv-colors/keyword-green :foreground ,vemv-default-autocomplete-popup-foreground-color))))
 `(cider-test-failure-face
   ((((class color) (background light))
     :background "firebrick"
     :foreground ,vemv-default-foreground-color)
    (((class color) (background dark))
     :background "firebrick"
     :foreground ,vemv-default-foreground-color)))
 `(cider-test-error-face
   ((((class color) (background light))
     :background "firebrick"
     :foreground ,vemv-default-foreground-color)
    (((class color) (background dark))
     :background "firebrick"
     :foreground ,vemv-default-foreground-color)))


 `(font-lock-builtin-face ((t (:foreground ,vemv-default-foreground-color))))

 ;; being developed
 `(clojure-global-constant-face ((t (:italic t :foreground ,vemv-colors/yellow))))
 `(clojure-def-symbol-face ((t (:foreground ,vemv-default-foreground-color))))
 `(clojure-lambda-arg-face ((t (:foreground ,vemv-default-foreground-color))))
 `(font-lock-comment-face ((t (:italic t :foreground ,vemv-colors/yellow))))
 `(font-lock-constant-face ((t (:italic t :foreground ,vemv-colors/keyword-green))))
 `(font-lock-function-name-face ((t (:foreground ,vemv-colors/yellow :bold t))))

 `(magit-diff-file-heading ((t (:foreground ,vemv-colors/yellow :bold t))))
 `(magit-diff-context-highlight ((t (:background ,vemv-default-background-color-slightly-darker))))

 ;; used for special forms and macros. also for def - which is annoying
 `(font-lock-keyword-face ((t (:foreground ,(if (eq :ruby vemv/project-type)
                                                vemv-default-foreground-color
                                              vemv-colors/lighter-yellow)))))
 `(font-lock-preprocessor-face ((t (:foreground ,vemv-colors/keyword-green)))) ;; Java
 `(font-lock-regexp-grouping-backslash ((t (:foreground ,vemv-colors/yellow))))
 `(font-lock-regexp-grouping-construct ((t (:foreground ,vemv-colors/blue))))
 `(font-lock-string-face ((t (:foreground ,vemv-colors/light-blue))))
 `(font-lock-doc-face ((t (:foreground ,vemv-colors/light-blue))))

 `(button ((t (:foreground ,vemv-default-foreground-color-very-slightly-darker :underline t))))

 ;; ns prefixes:
 `(font-lock-type-face ((t (:foreground ,vemv-default-foreground-color-very-slightly-darker
                                        :background ,vemv-default-background-color-slightly-darker))))

 `(font-lock-variable-name-face ((t ,(if (eq :ruby vemv/project-type)
                                         `(:foreground ,vemv-default-foreground-color)
                                       `(:foreground ,vemv-colors/yellow :bold t)))))
 `(font-lock-warning-face ((t (:bold t :background ,vemv-colors/warning-pink :foreground "white"))))

 `(flycheck-fringe-warning ((t (:foreground "#000000"))))
 `(flycheck-fringe-info    ((t (:foreground "#000000"))))
 `(flycheck-info           ((t (:underline (:color ,vemv-colors/yellow :style wave)))))
 `(flycheck-warning        ((t (:underline (:color ,vemv-colors/yellow :style wave)))))

 `(gui-element ((t (:background "#484848" :foreground "#96CBFE"))))
 `(region ((t (:background ,vemv-colors/purple))))
 `(mode-line ((t (:background "gray10" :foreground "#B6B6B6"))))
 `(mode-line-inactive ((t (:background "gray10" :foreground "#696969"))))

 `(highlight ((t (:background "#444F57"))))
 `(isearch ((t (:background "deep pink" :foreground "black"))))
 `(isearch-fail ((t (:background "red4"))))
 `(lazy-highlight ((t (:background "yellow" :foreground "black"))))
 `(query-replace ((t (:background ,vemv-default-autocomplete-popup-foreground-color))))
 `(Highline-face ((t (:background "SeaGreen"))))
 `(italic ((t (nil))))
 `(left-margin ((t (nil))))
 `(text-cursor ((t (:background "yellow" :foreground "black"))))
 `(toolbar ((t (nil))))
 `(underline ((nil (:underline nil))))
 `(vertical-border ((t (:background "black" :foreground ,vemv-default-autocomplete-popup-foreground-color))))
 `(zmacs-region ((t (:background "snow" :foreground "ble"))))

 `(diff-added ((t (:foreground "green"))))
 `(diff-removed ((t (:foreground "red"))))

 `(magit-diff-add ((t (:foreground "green"))))
 `(magit-diff-del ((t (:foreground "red"))))
 `(magit-item-highlight ((t (:background "gray15"))))
 `(magit-section-title ((t (:foreground "deep pink"))))
 `(magit-diff-hunk-header ((t (:foreground "orange"))))
 `(magit-branch ((t (:foreground "gold"))))

 `(eval-sexp-fu-flash ((t (:background "grey15" :foreground "DeepPink3"))))

 `(ac-completion-face ((t (:foreground "darkgray" :underline t :slant normal :weight normal))))
 `(ac-candidate-face ((t (:background ,vemv-default-autocomplete-popup-background-color :foreground "black"
                                      :slant normal :weight normal))))
 `(ac-selection-face ((t (:background "deep pink" :foreground "black" :slant normal :weight normal))))

 `(company-tooltip ((t (:background ,vemv-default-autocomplete-popup-background-color
                                    :foreground ,vemv-default-autocomplete-popup-foreground-color
                                    :slant normal :weight normal)))) ;; main
 `(company-tooltip-mouse ((t (:background ,vemv-default-autocomplete-popup-background-color-lighter
                                          :foreground ,vemv-default-autocomplete-popup-foreground-color))))
 `(company-tooltip-search ((t (:background ,vemv-default-autocomplete-popup-background-color
                                           :foreground ,vemv-default-autocomplete-popup-foreground-color
                                           :slant normal
                                           :weight normal)))) ;; the 'match' so far
 `(company-tooltip-annotation ((t (:background ,vemv-default-autocomplete-popup-background-color
                                               :foreground ,vemv-default-autocomplete-popup-foreground-color
                                               :slant normal
                                               :weight normal))))
 `(company-tooltip-annotation-selection ((t (:background ,vemv-default-autocomplete-popup-background-color
                                                         :foreground ,vemv-default-autocomplete-popup-foreground-color
                                                         :slant normal :weight normal))))
 `(company-tooltip-common-selection ((t (:background "deep pink"
                                                     :foreground ,vemv-default-autocomplete-popup-foreground-color
                                                     :slant normal :weight normal))))
 `(company-tooltip-common ((t (:background ,vemv-default-autocomplete-popup-background-color
                                           :foreground ,vemv-default-autocomplete-popup-foreground-color
                                           :slant normal :weight normal))))
 `(company-tooltip-selection ((t (:background "deep pink" :foreground ,vemv-default-autocomplete-popup-foreground-color
                                              :slant normal
                                              :weight normal)))) ;; the completion that one is currently choosing
 `(company-preview-common ((t (:background "deep pink" :foreground ,vemv-default-autocomplete-popup-foreground-color
                                           :slant normal :weight normal)))) ;; the completion that one is currently choosing
 `(company-preview-search ((t (:background "deep pink" :foreground ,vemv-default-autocomplete-popup-foreground-color
                                           :slant normal :weight normal)))) ;; the completion that one is currently choosing
 `(company-preview ((t (:background "deep pink" :foreground ,vemv-default-autocomplete-popup-foreground-color
                                    :slant normal :weight normal)))) ;; the completion that one is currently choosing
 `(company-scrollbar-fg ((t (:background "#696969"))))
 `(company-scrollbar-bg ((t (:background "#B6B6B6"))))

 `(popup-isearch-match ((t (:background "black" :foreground "deep pink" :slant normal :weight normal))))
 `(popup-tip-face ((t (:background ,vemv-default-autocomplete-popup-foreground-color
                                   :foreground "whitepnpnn" :slant normal :weight normal))))
 `(popup-scroll-bar-foreground-face ((t (:background "#0A0A0A"))))
 `(popup-scroll-bar-background-face ((t (:background ,vemv-default-autocomplete-popup-foreground-color))))

 `(window-number-face ((t (:background "grey10" :foreground "#696969"))))

 `(yas/field-highlight-face ((t (:background "deep pink" :foreground "black"))))

 ;; Emacs 25:
 `(show-paren-match-face ((t ,(if (eq :ruby vemv/project-type)
                                  `(:foreground ,vemv-default-foreground-color :background ,vemv-colors/purple)
                                `(:background ,vemv-colors/pink :foreground ,vemv-default-foreground-color)))))

 ;; Emacs 26:
 `(show-paren-match ((t ,(if (eq :ruby vemv/project-type)
                             `(:foreground ,vemv-default-foreground-color :background ,vemv-colors/purple)
                           `(:background ,vemv-colors/pink :foreground ,vemv-default-foreground-color)))))

 `(helm-candidate-number ((t (:inherit helm-header))))
 `(helm-candidate-number-suspended ((t (:inherit helm-header))))
 `(helm-ff-directory ((t (:foreground ,vemv-colors/yellow :bold t))))
 `(helm-ff-dotted-directory ((t (:foreground ,vemv-colors/yellow :bold t))))
 `(helm-ff-dotted-symlink-directory ((t (:foreground ,vemv-colors/yellow :bold t))))
 `(helm-ff-prefix ((t (:inherit helm-header))))
 `(helm-ff-symlink ((t (:foreground ,vemv-colors/light-blue))))
 `(helm-grep-lineno ((t (:foreground ,vemv-colors/lighter-yellow))))
 `(helm-grep-finish ((t (:inherit mode-line))))
 `(helm-grep-match ((t (:foreground ,vemv-colors/keyword-green))))
 `(helm-header-line-left-margin ((t (:inherit helm-header))))
 `(helm-match ((t (:foreground ,vemv-colors/keyword-green :background ,vemv-default-autocomplete-popup-foreground-color))))
 `(helm-match-item ((t (:foreground ,vemv-colors/keyword-green))))
 `(helm-moccur-buffer ((t (:foreground ,vemv-default-foreground-color-very-slightly-darker
                                       :background ,vemv-default-background-color-slightly-darker))))
 `(helm-selection ((t (:background ,vemv-default-background-color-slightly-darker))))
 `(helm-selection ((t (:background ,vemv-colors/purple :foreground ,vemv-default-foreground-color))))
 `(helm-separator ((t (:foreground ,vemv-colors/lighter-yellow))))
 `(helm-source-header ((t (:inherit helm-header :bold t))))
 `(helm-visible-mark ((t (:background "#D93273" :foreground ,vemv-default-foreground-color))))

 `(naeu-green-face ((t (:foreground "green" :background "black"))))
 `(naeu-pink-face ((t (:foreground "deep pink" :background "black"))))
 `(naeu-blue-face ((t (:foreground "medium slate blue" :background "black"))))
 `(naeu-orange-face ((t (:foreground ,vemv-colors/yellow :background "black"))))
 `(naeu-red-face ((t (:foreground "orange" :background "black"))))
 `(naeu-grey-face ((t (:foreground "gray30" :background "black"))))

 `(ido-first-match ((t (:foreground "deep pink" :background "black"))))
 `(ido-only-match ((t (:foreground "deep pink" :background "black"))))
 `(ido-subdir ((t (:foreground "gray60" :background "black"))))
 `(ido-indicator ((t (:foreground "black" :background "deep pink"))))

 `(match ((t (:foreground "deep pink" :background "blackn"))))
 `(minibuffer-prompt ((t (:foreground ,vemv-colors/keyword-green  :background "black"))))
 `(grep-match-face ((t (:foreground "black" :background "deep pink"))))
 `(grep-hit-face ((t (:foreground "black" :background "red"))))
 `(grep-context-face ((t (:foreground "black" :background "deep pink"))))

 `(erc-notice-face ((t (:bold t :foreground "grey26"))))

 `(vhl/default-face ((t (:background ,vemv-default-autocomplete-popup-foreground-color))))
 `(undo-tree-visualizer-active-branch-face ((t (:foreground "deep pink" :background "black")))))

(setq ansi-color-faces-vector [vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face])

(setq ansi-color-map          [vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face])

(setq ansi-color-names-vector [vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face])

(setq ansi-term-color-vector  [vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face
                               vemv-default-face])
