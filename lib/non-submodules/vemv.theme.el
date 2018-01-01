;; for debugging:
;;   list-faces-display
;;   m-x describe-theme
;;   (buffer-string)

(provide 'vemv.theme)

(set-face-background 'comint-highlight-prompt "#4D575F")
(set-face-foreground 'comint-highlight-prompt "white")

(deftheme vemv "")

(setq vemv-colors/keyword-green "#C1ED3D")

(defface font-lock-line-and-column-face
  '((t :foreground "#696969"))
  "."
  :group 'vemv)
  
(defface vemv-cider-connection-face
  '((t :foreground "#CFCFCF"))
  "."
  :group 'vemv)

(setq vemv-default-background-color "#4D575F")
(setq vemv-default-foreground-color "#F5F5F5")
(setq vemv-error-foreground-color vemv-default-foreground-color)

(defface vemv-default-face
  `((t :foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))
  "."
  :group 'vemv)


(custom-theme-set-faces 'vemv
      
      `(term-color-black      ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
      `(term-color-red        ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
      `(term-color-green      ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
      `(term-color-yellow     ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
      `(term-color-blue       ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
      `(term-color-magenta    ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
      `(term-color-cyan       ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
      `(term-color-white      ((t (:foreground ,vemv-default-foreground-color :background ,vemv-default-background-color))))
      '(term-default-fg-color ((t (:inherit term-color-white))))
      '(term-default-bg-color ((t (:inherit term-color-black))))
      
      '(grizzl-selection-face
       ((((class color) (background light))
         (:foreground "#C1ED3D"))
        (((class color) (background dark))
         (:foreground "#C1ED3D"))
        (t (:foreground "#C1ED3D"))))
      
      `(default ((t (:background ,vemv-default-background-color :foreground ,vemv-default-foreground-color))))
      `(default ((t (:background ,vemv-default-background-color :foreground ,vemv-default-foreground-color))))
      '(cursor ((t (:foreground "#4D575F" :background "#CFCFCF"))))
      '(blue ((t (:foreground "blue"))))
      '(bold ((t (:bold t))))
      '(bold-italic ((t (:bold t :italc t))))
      '(border-glyph ((t (nil))))
      '(buffers-tab ((t (:foreground "#F8F8F8"))))
      
      `(cider-fringe-good-face ((t (:foreground ,vemv-default-foreground-color))))
      `(cider-error-highlight-face ((t (:foreground ,vemv-default-foreground-color))))
      `(cider-stacktrace-error-class-face ((t (:foreground ,vemv-error-foreground-color))))
      `(cider-repl-stderr-face ((t (:foreground ,vemv-error-foreground-color))))
      `(cider-stacktrace-error-class-face ((t (:foreground ,vemv-error-foreground-color))))
      
      '(font-lock-builtin-face ((t (:foreground "#F5F5F5"))))
      '(font-lock-comment-face ((t (:italic t :foreground "#FFE33B"))))
      `(font-lock-constant-face ((t (:italic t :foreground ,vemv-colors/keyword-green))))
      '(font-lock-doc-string-face ((t (:foreground "#FFE33B" :italic t))))
      '(font-lock-function-name-face ((t (:foreground "#FFE33B" :bold t))))
      
      ;; used for special forms and macros. also for def - which is annoying
      '(font-lock-keyword-face ((t (:foreground "#ffe444")))) ;; a lighter yellow
      '(font-lock-preprocessor-face ((t (:foreground "#C1ED3D")))) ;; Java
      ;; '(font-lock-reference-face ((t (:foreground "medium slate blue"))))
      ;; '(font-lock-reference-face ((t (:foreground "gray"))))
      '(font-lock-regexp-grouping-backslash ((t (:foreground "#FFE33B"))))
      '(font-lock-regexp-grouping-construct ((t (:foreground "red"))))
      '(font-lock-string-face ((t (:foreground "#BFD8FF"))))
      
      ;; ns prefixes:
      '(font-lock-type-face ((t (:foreground "#ededed"))))
      
      '(font-lock-variable-name-face ((t (:foreground "#FFE33B" :bold t))))
      '(font-lock-warning-face ((t (:bold t :foreground "Pink"))))

      '(gui-element ((t (:background "#484848" :foreground "#96CBFE"))))
      '(region ((t ( :background "#7A3555"))))
      '(mode-line ((t (:background "gray10" :foreground "#B6B6B6"))))
      '(mode-line-inactive ((t (:background "gray10" :foreground "#696969"))))

      '(highlight ((t (:background "#444F57"))))
      '(isearch ((t (:background "deep pink" :foreground "black"))))
      '(isearch-fail ((t (:background "red4"))))
      '(lazy-highlight ((t (:background "yellow" :foreground "black"))))
      '(query-replace ((t (:background "#333333"))))
      '(Highline-face ((t (:background "SeaGreen"))))
      '(italic ((t (nil))))
      '(left-margin ((t (nil))))
      '(text-cursor ((t (:background "yellow" :foreground "black"))))
      '(toolbar ((t (nil))))
      '(underline ((nil (:underline nil))))
      '(vertical-border ((t (:background "black" :foreground "#333333"))))
      '(zmacs-region ((t (:background "snow" :foreground "ble"))))

      '(diff-added ((t (:foreground "green"))))
      '(diff-removed ((t (:foreground "red"))))

      '(magit-diff-add ((t (:foreground "green"))))
      '(magit-diff-del ((t (:foreground "red"))))
      '(magit-item-highlight ((t (:background "gray15"))))
      '(magit-section-title ((t (:foreground "deep pink"))))
      '(magit-diff-hunk-header ((t (:foreground "orange"))))
      '(magit-branch ((t (:foreground "gold"))))

      '(eval-sexp-fu-flash ((t (:background "grey15" :foreground "DeepPink3"))))

      '(ac-completion-face ((t (:foreground "darkgray" :underline t :slant normal :weight normal))))
      '(ac-candidate-face ((t (:background "#A2A2A2" :foreground "black" :slant normal :weight normal))))
      '(ac-selection-face ((t (:background "deep pink" :foreground "black" :slant normal :weight normal))))
      
      '(company-tooltip ((t (:background "#A2A2A2" :foreground "#333333" :slant normal :weight normal)))) ;; main
      '(company-tooltip-search ((t (:background "#A2A2A2" :foreground "#333333" :slant normal :weight normal)))) ;; the 'match' so far
      '(company-tooltip-annotation ((t (:background "#A2A2A2" :foreground "#333333" :slant normal :weight normal))))
      '(company-tooltip-annotation-selection ((t (:background "#A2A2A2" :foreground "#333333" :slant normal :weight normal))))
      '(company-tooltip-common-selection ((t (:background "deep pink" :foreground "#333333" :slant normal :weight normal))))
      '(company-tooltip-common ((t (:background "#A2A2A2" :foreground "#333333" :slant normal :weight normal))))
      '(company-tooltip-selection ((t (:background "deep pink" :foreground "#333333" :slant normal :weight normal)))) ;; the completion that one is currently choosing
      '(company-preview-common ((t (:background "deep pink" :foreground "#333333" :slant normal :weight normal)))) ;; the completion that one is currently choosing
      '(company-preview-search ((t (:background "deep pink" :foreground "#333333" :slant normal :weight normal)))) ;; the completion that one is currently choosing
      '(company-preview ((t (:background "deep pink" :foreground "#333333" :slant normal :weight normal)))) ;; the completion that one is currently choosing
      '(company-scrollbar-fg ((t (:background "#696969"))))
      '(company-scrollbar-bg ((t (:background "#B6B6B6"))))
      
      
      
      '(popup-isearch-match ((t (:background "black" :foreground "deep pink" :slant normal :weight normal))))
      '(popup-tip-face ((t (:background "#333333" :foreground "whitepnpnn" :slant normal :weight normal))))
      '(popup-scroll-bar-foreground-face ((t (:background "#0A0A0A"))))
      '(popup-scroll-bar-background-face ((t (:background "#333333"))))

      '(window-number-face ((t (:background "grey10" :foreground "#696969"))))

      '(yas/field-highlight-face ((t (:background "deep pink" :foreground "black"))))

      '(show-paren-match-face ((t (:background "#D93273" :foreground "#F5F5F5"))))

      '(naeu-green-face ((t (:foreground "green" :background "black"))))
      '(naeu-pink-face ((t (:foreground "deep pink" :background "black"))))
      '(naeu-blue-face ((t (:foreground "medium slate blue" :background "black"))))
      '(naeu-orange-face ((t (:foreground "#FFE33B" :background "black"))))
      '(naeu-red-face ((t (:foreground "orange" :background "black"))))
      '(naeu-grey-face ((t (:foreground "gray30" :background "black"))))

      '(ido-first-match ((t (:foreground "deep pink" :background "black"))))
      '(ido-only-match ((t (:foreground "deep pink" :background "black"))))
      '(ido-subdir ((t (:foreground "gray60" :background "black"))))
      '(ido-indicator ((t (:foreground "black" :background "deep pink"))))

      '(match ((t (:foreground "deep pink" :background "blackn"))))
      '(minibuffer-prompt ((t (:foreground "#61CE3C" :background "black"))))
      '(grep-match-face ((t (:foreground "black" :background "deep pink"))))
      '(grep-hit-face ((t (:foreground "black" :background "red"))))
      '(grep-context-face ((t (:foreground "black" :background "deep pink"))))

      '(erc-notice-face ((t (:bold t :foreground "grey26"))))

      '(erc-action-face ((t (:foreground "#FF6400"))))
      '(erc-current-nick-face ((t (:foreground "#FFE33B"))))
      '(erc-dangerous-host-face ((t (:foreground "red"))))
      '(erc-default-face ((t (:foreground "#61CE3C"))))
      '(erc-direct-msg-face ((t (:foreground "orange"))))
      '(erc-error-face ((t (:foreground "red"))))
      '(erc-fool-face ((t (:foreground "dim gray"))))
      '(erc-header-line ((t (:background "grey90" :foreground "grey20"))))
      '(erc-input-face ((t (:foreground "#4c83ff"))))
      '(erc-inverse-face ((t (:background "Black" :foreground "White"))))
      '(erc-keyword-face ((t (:foreground "deep pink"))))
      '(erc-my-nick-face ((t (:bold t :foreground "deep pink" ))))
      '(erc-nick-default-face ((t (:foreground "grey57"))))
      '(erc-nick-msg-face ((t (:foreground "deep pink"))))

      '(erc-pal-face ((t (:bold t :foreground "Magenta" :weight bold))))
      '(erc-prompt-face ((t (:bold t :background "lightBlue2" :foreground "Black" :weight bold))))
      '(erc-timestamp-face ((t (:foreground "dim gray"))))
      '(erc-underline-face ((t (:underline t))))

      '(vhl/default-face ((t (:background "#333333"))))
      '(undo-tree-visualizer-active-branch-face ((t (:foreground "deep pink" :background "black")))))

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
