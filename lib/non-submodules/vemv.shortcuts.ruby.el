(require 'vemv.lang)
(require 'vemv.shortcuts.ruby.base)
(provide 'vemv.shortcuts.ruby)

(setq vemv/shortcuts/ruby/secondary-up            'sp-splice-sexp
      vemv/shortcuts/ruby/primary-backspace       'kill-line
      vemv/shortcuts/ruby/secondary-k             (argless
                                                   (call-interactively 'sp-backward-kill-sexp))
      vemv/shortcuts/ruby/secondary-backspace     (argless
                                                   (call-interactively 'sp-backward-kill-word)))
