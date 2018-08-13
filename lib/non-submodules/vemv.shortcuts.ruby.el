(require 'vemv.lang)
(require 'vemv.shortcuts.ruby.base)
(provide 'vemv.shortcuts.ruby)

(setq vemv/shortcuts/ruby/secondary-up               (argless
                                                      (call-interactively 'sp-splice-sexp))
      vemv/shortcuts/ruby/primary-backspace          'kill-line
      vemv/shortcuts/ruby/secondary-k                (argless
                                                      (call-interactively 'sp-backward-kill-sexp))
      vemv/shortcuts/ruby/secondary-backspace        (argless
                                                      (call-interactively 'sp-backward-kill-word))
      vemv/shortcuts/ruby/secondary-left-bracket     (argless
                                                      (call-interactively 'sp-backward-sexp))
      vemv/shortcuts/ruby/secondary-left-parens      (argless
                                                      (call-interactively 'sp-wrap-round))
      vemv/shortcuts/ruby/secondary-right-bracket    (argless
                                                      (call-interactively 'sp-forward-sexp)))
