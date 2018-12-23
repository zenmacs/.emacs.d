(require 'vemv.lang)
(provide 'vemv.data.bindings)
(require 'vemv.shortcuts.global)
(require 'vemv.shortcuts.clojure)
(require 'vemv.shortcuts.ruby)
;; generated with gen.rb

(setq vemv/exhaustive-list-of-bindings-to-remove (list
                                                       "<escape>-<escape>-<escape>"
                                                       ";"
                                                       [f1]
                                                       [f10]
                                                       [f11]
                                                       [f12]
                                                       [f2]
                                                       [f3]
                                                       [f4]
                                                       [f5]
                                                       [f6]
                                                       [f7]
                                                       [f8]
                                                       [f9]
                                                       "<backspace>"
                                                       "<down>"
                                                       "<end>"
                                                       "<home>"
                                                       "<left>"
                                                       "<next>"
                                                       "<prior>"
                                                       "<right>"
                                                       "<up>"
                                                       "RET"
                                                       "C-a"
                                                       "C-S-a"
                                                       "C-M-a"
                                                       "C-b"
                                                       "C-S-b"
                                                       "C-M-b"
                                                       "C-c"
                                                       "C-S-c"
                                                       "C-M-c"
                                                       "C-d"
                                                       "C-S-d"
                                                       "C-M-d"
                                                       "C-e"
                                                       "C-S-e"
                                                       "C-M-e"
                                                       "C-f"
                                                       "C-S-f"
                                                       "C-M-f"
                                                       "C-h"
                                                       "C-S-h"
                                                       "C-M-h"
                                                       "C-j"
                                                       "C-S-j"
                                                       "C-M-j"
                                                       "C-k"
                                                       "C-S-k"
                                                       "C-M-k"
                                                       "C-l"
                                                       "C-S-l"
                                                       "C-M-l"
                                                       "C-n"
                                                       "C-S-n"
                                                       "C-M-n"
                                                       "C-o"
                                                       "C-S-o"
                                                       "C-M-o"
                                                       "C-p"
                                                       "C-S-p"
                                                       "C-M-p"
                                                       "C-q"
                                                       "C-S-q"
                                                       "C-M-q"
                                                       "C-r"
                                                       "C-S-r"
                                                       "C-M-r"
                                                       "C-s"
                                                       "C-S-s"
                                                       "C-M-s"
                                                       "C-t"
                                                       "C-S-t"
                                                       "C-M-t"
                                                       "C-u"
                                                       "C-S-u"
                                                       "C-M-u"
                                                       "C-v"
                                                       "C-S-v"
                                                       "C-M-v"
                                                       "C-w"
                                                       "C-S-w"
                                                       "C-M-w"
                                                       "C-x"
                                                       "C-S-x"
                                                       "C-M-x"
                                                       "C-y"
                                                       "C-S-y"
                                                       "C-M-y"
                                                       "C-z"
                                                       "C-S-z"
                                                       "C-M-z"
                                                       "C-0"
                                                       "C-S-0"
                                                       "C-M-0"
                                                       "C-1"
                                                       "C-S-1"
                                                       "C-M-1"
                                                       "C-2"
                                                       "C-S-2"
                                                       "C-M-2"
                                                       "C-3"
                                                       "C-S-3"
                                                       "C-M-3"
                                                       "C-4"
                                                       "C-S-4"
                                                       "C-M-4"
                                                       "C-5"
                                                       "C-S-5"
                                                       "C-M-5"
                                                       "C-6"
                                                       "C-S-6"
                                                       "C-M-6"
                                                       "C-7"
                                                       "C-S-7"
                                                       "C-M-7"
                                                       "C-8"
                                                       "C-S-8"
                                                       "C-M-8"
                                                       "C-9"
                                                       "C-S-9"
                                                       "C-M-9"
                                                       "C-!"
                                                       "C-S-!"
                                                       "C-M-!"
                                                       "C-@"
                                                       "C-S-@"
                                                       "C-M-@"
                                                       "C-&"
                                                       "C-S-&"
                                                       "C-M-&"
                                                       "C-#"
                                                       "C-S-#"
                                                       "C-M-#"
                                                       "C-%"
                                                       "C-S-%"
                                                       "C-M-%"
                                                       "C-^"
                                                       "C-S-^"
                                                       "C-M-^"
                                                       "C-$"
                                                       "C-S-$"
                                                       "C-M-$"
                                                       "C-_"
                                                       "C-S-_"
                                                       "C-M-_"
                                                       "C--"
                                                       "C-S--"
                                                       "C-M--"
                                                       "C-,"
                                                       "C-S-,"
                                                       "C-M-,"
                                                       "C-;"
                                                       "C-S-;"
                                                       "C-M-;"
                                                       "C-:"
                                                       "C-S-:"
                                                       "C-M-:"
                                                       "C-?"
                                                       "C-S-?"
                                                       "C-M-?"
                                                       "C-."
                                                       "C-S-."
                                                       "C-M-."
                                                       "C-'"
                                                       "C-S-'"
                                                       "C-M-'"
                                                       "C-("
                                                       "C-S-("
                                                       "C-M-("
                                                       "C-)"
                                                       "C-S-)"
                                                       "C-M-)"
                                                       "C-]"
                                                       "C-S-]"
                                                       "C-M-]"
                                                       "C-{"
                                                       "C-S-{"
                                                       "C-M-{"
                                                       "C-}"
                                                       "C-S-}"
                                                       "C-M-}"
                                                       "C-*"
                                                       "C-S-*"
                                                       "C-M-*"
                                                       "C-/"
                                                       "C-S-/"
                                                       "C-M-/"
                                                       "C-`"
                                                       "C-S-`"
                                                       "C-M-`"
                                                       "C-+"
                                                       "C-S-+"
                                                       "C-M-+"
                                                       "C-<backspace>"
                                                       "C-S-<backspace>"
                                                       "C-M-<backspace>"
                                                       "C-<down>"
                                                       "C-S-<down>"
                                                       "C-M-<down>"
                                                       "C-<end>"
                                                       "C-S-<end>"
                                                       "C-M-<end>"
                                                       "C-<home>"
                                                       "C-S-<home>"
                                                       "C-M-<home>"
                                                       "C-<left>"
                                                       "C-S-<left>"
                                                       "C-M-<left>"
                                                       "C-<next>"
                                                       "C-S-<next>"
                                                       "C-M-<next>"
                                                       "C-<prior>"
                                                       "C-S-<prior>"
                                                       "C-M-<prior>"
                                                       "C-<right>"
                                                       "C-S-<right>"
                                                       "C-M-<right>"
                                                       "C-<up>"
                                                       "C-S-<up>"
                                                       "C-M-<up>"
                                                       "C-="
                                                       "C-S-="
                                                       "C-M-="
                                                       "C-|"
                                                       "C-S-|"
                                                       "C-M-|"
                                                       "C-RET"
                                                       "C-S-RET"
                                                       "C-M-RET"
                                                       "C-SPC"
                                                       "C-S-SPC"
                                                       "C-M-SPC"
                                                       "M-a"
                                                       "M-S-a"
                                                       "M-b"
                                                       "M-S-b"
                                                       "M-c"
                                                       "M-S-c"
                                                       "M-d"
                                                       "M-S-d"
                                                       "M-e"
                                                       "M-S-e"
                                                       "M-f"
                                                       "M-S-f"
                                                       "M-g"
                                                       "M-S-g"
                                                       "M-h"
                                                       "M-S-h"
                                                       "M-i"
                                                       "M-S-i"
                                                       "M-j"
                                                       "M-S-j"
                                                       "M-k"
                                                       "M-S-k"
                                                       "M-l"
                                                       "M-S-l"
                                                       "M-m"
                                                       "M-S-m"
                                                       "M-n"
                                                       "M-S-n"
                                                       "M-o"
                                                       "M-S-o"
                                                       "M-p"
                                                       "M-S-p"
                                                       "M-q"
                                                       "M-S-q"
                                                       "M-r"
                                                       "M-S-r"
                                                       "M-s"
                                                       "M-S-s"
                                                       "M-t"
                                                       "M-S-t"
                                                       "M-u"
                                                       "M-S-u"
                                                       "M-v"
                                                       "M-S-v"
                                                       "M-w"
                                                       "M-S-w"
                                                       "M-x"
                                                       "M-S-x"
                                                       "M-y"
                                                       "M-S-y"
                                                       "M-z"
                                                       "M-S-z"
                                                       "M-0"
                                                       "M-S-0"
                                                       "M-1"
                                                       "M-S-1"
                                                       "M-2"
                                                       "M-S-2"
                                                       "M-3"
                                                       "M-S-3"
                                                       "M-4"
                                                       "M-S-4"
                                                       "M-5"
                                                       "M-S-5"
                                                       "M-6"
                                                       "M-S-6"
                                                       "M-7"
                                                       "M-S-7"
                                                       "M-8"
                                                       "M-S-8"
                                                       "M-9"
                                                       "M-S-9"
                                                       "M-!"
                                                       "M-S-!"
                                                       "M-@"
                                                       "M-S-@"
                                                       "M-&"
                                                       "M-S-&"
                                                       "M-#"
                                                       "M-S-#"
                                                       "M-%"
                                                       "M-S-%"
                                                       "M-^"
                                                       "M-S-^"
                                                       "M-~"
                                                       "M-S-~"
                                                       "M-$"
                                                       "M-S-$"
                                                       "M-_"
                                                       "M-S-_"
                                                       "M--"
                                                       "M-S--"
                                                       "M-,"
                                                       "M-S-,"
                                                       "M-;"
                                                       "M-S-;"
                                                       "M-:"
                                                       "M-S-:"
                                                       "M-?"
                                                       "M-S-?"
                                                       "M-."
                                                       "M-S-."
                                                       "M-'"
                                                       "M-S-'"
                                                       "M-("
                                                       "M-S-("
                                                       "M-)"
                                                       "M-S-)"
                                                       "M-["
                                                       "M-S-["
                                                       "M-]"
                                                       "M-S-]"
                                                       "M-{"
                                                       "M-S-{"
                                                       "M-}"
                                                       "M-S-}"
                                                       "M-*"
                                                       "M-S-*"
                                                       "M-/"
                                                       "M-S-/"
                                                       "M-`"
                                                       "M-S-`"
                                                       "M-+"
                                                       "M-S-+"
                                                       "M-<backspace>"
                                                       "M-S-<backspace>"
                                                       "M-<down>"
                                                       "M-S-<down>"
                                                       "M-<end>"
                                                       "M-S-<end>"
                                                       "M-<home>"
                                                       "M-S-<home>"
                                                       "M-<left>"
                                                       "M-S-<left>"
                                                       "M-<next>"
                                                       "M-S-<next>"
                                                       "M-<prior>"
                                                       "M-S-<prior>"
                                                       "M-<right>"
                                                       "M-S-<right>"
                                                       "M-<up>"
                                                       "M-S-<up>"
                                                       "M-="
                                                       "M-S-="
                                                       "M-|"
                                                       "M-S-|"
                                                       "M-RET"
                                                       "M-S-RET"
                                                       "M-SPC"
                                                       "M-S-SPC"
                                                       "s-a"
                                                       "s-S-a"
                                                       "s-b"
                                                       "s-S-b"
                                                       "s-c"
                                                       "s-S-c"
                                                       "s-d"
                                                       "s-S-d"
                                                       "s-e"
                                                       "s-S-e"
                                                       "s-f"
                                                       "s-S-f"
                                                       "s-g"
                                                       "s-S-g"
                                                       "s-h"
                                                       "s-S-h"
                                                       "s-i"
                                                       "s-S-i"
                                                       "s-j"
                                                       "s-S-j"
                                                       "s-k"
                                                       "s-S-k"
                                                       "s-l"
                                                       "s-S-l"
                                                       "s-m"
                                                       "s-S-m"
                                                       "s-n"
                                                       "s-S-n"
                                                       "s-o"
                                                       "s-S-o"
                                                       "s-p"
                                                       "s-S-p"
                                                       "s-q"
                                                       "s-S-q"
                                                       "s-r"
                                                       "s-S-r"
                                                       "s-s"
                                                       "s-S-s"
                                                       "s-t"
                                                       "s-S-t"
                                                       "s-u"
                                                       "s-S-u"
                                                       "s-v"
                                                       "s-S-v"
                                                       "s-w"
                                                       "s-S-w"
                                                       "s-x"
                                                       "s-S-x"
                                                       "s-y"
                                                       "s-S-y"
                                                       "s-z"
                                                       "s-S-z"
                                                       "s-0"
                                                       "s-S-0"
                                                       "s-1"
                                                       "s-S-1"
                                                       "s-2"
                                                       "s-S-2"
                                                       "s-3"
                                                       "s-S-3"
                                                       "s-4"
                                                       "s-S-4"
                                                       "s-5"
                                                       "s-S-5"
                                                       "s-6"
                                                       "s-S-6"
                                                       "s-7"
                                                       "s-S-7"
                                                       "s-8"
                                                       "s-S-8"
                                                       "s-9"
                                                       "s-S-9"
                                                       "s-!"
                                                       "s-S-!"
                                                       "s-@"
                                                       "s-S-@"
                                                       "s-&"
                                                       "s-S-&"
                                                       "s-#"
                                                       "s-S-#"
                                                       "s-%"
                                                       "s-S-%"
                                                       "s-^"
                                                       "s-S-^"
                                                       "s-~"
                                                       "s-S-~"
                                                       "s-$"
                                                       "s-S-$"
                                                       "s-_"
                                                       "s-S-_"
                                                       "s--"
                                                       "s-S--"
                                                       "s-,"
                                                       "s-S-,"
                                                       "s-;"
                                                       "s-S-;"
                                                       "s-:"
                                                       "s-S-:"
                                                       "s-?"
                                                       "s-S-?"
                                                       "s-."
                                                       "s-S-."
                                                       "s-'"
                                                       "s-S-'"
                                                       "s-("
                                                       "s-S-("
                                                       "s-)"
                                                       "s-S-)"
                                                       "s-["
                                                       "s-S-["
                                                       "s-]"
                                                       "s-S-]"
                                                       "s-{"
                                                       "s-S-{"
                                                       "s-}"
                                                       "s-S-}"
                                                       "s-*"
                                                       "s-S-*"
                                                       "s-/"
                                                       "s-S-/"
                                                       "s-`"
                                                       "s-S-`"
                                                       "s-+"
                                                       "s-S-+"
                                                       "s-<backspace>"
                                                       "s-S-<backspace>"
                                                       "s-<down>"
                                                       "s-S-<down>"
                                                       "s-<end>"
                                                       "s-S-<end>"
                                                       "s-<home>"
                                                       "s-S-<home>"
                                                       "s-<left>"
                                                       "s-S-<left>"
                                                       "s-<next>"
                                                       "s-S-<next>"
                                                       "s-<prior>"
                                                       "s-S-<prior>"
                                                       "s-<right>"
                                                       "s-S-<right>"
                                                       "s-<up>"
                                                       "s-S-<up>"
                                                       "s-="
                                                       "s-S-="
                                                       "s-|"
                                                       "s-S-|"
                                                       "s-RET"
                                                       "s-S-RET"
                                                       "s-SPC"
                                                       "s-S-SPC"))

(setq vemv/global-key-bindings
  (vemv/hash-map
    [f1] (argless (if vemv/shortcuts/global/f1 (vemv/keyboard-funcall :vemv/shortcuts/global/f1 vemv/shortcuts/global/f1)))
    "<s-f1>" (argless (if vemv/shortcuts/global/S-f1 (vemv/keyboard-funcall :vemv/shortcuts/global/S-f1 vemv/shortcuts/global/S-f1)))
    [f10] (argless (if vemv/shortcuts/global/f10 (vemv/keyboard-funcall :vemv/shortcuts/global/f10 vemv/shortcuts/global/f10)))
    "<s-f10>" (argless (if vemv/shortcuts/global/S-f10 (vemv/keyboard-funcall :vemv/shortcuts/global/S-f10 vemv/shortcuts/global/S-f10)))
    [f11] (argless (if vemv/shortcuts/global/f11 (vemv/keyboard-funcall :vemv/shortcuts/global/f11 vemv/shortcuts/global/f11)))
    "<s-f11>" (argless (if vemv/shortcuts/global/S-f11 (vemv/keyboard-funcall :vemv/shortcuts/global/S-f11 vemv/shortcuts/global/S-f11)))
    [f12] (argless (if vemv/shortcuts/global/f12 (vemv/keyboard-funcall :vemv/shortcuts/global/f12 vemv/shortcuts/global/f12)))
    "<s-f12>" (argless (if vemv/shortcuts/global/S-f12 (vemv/keyboard-funcall :vemv/shortcuts/global/S-f12 vemv/shortcuts/global/S-f12)))
    [f2] (argless (if vemv/shortcuts/global/f2 (vemv/keyboard-funcall :vemv/shortcuts/global/f2 vemv/shortcuts/global/f2)))
    "<s-f2>" (argless (if vemv/shortcuts/global/S-f2 (vemv/keyboard-funcall :vemv/shortcuts/global/S-f2 vemv/shortcuts/global/S-f2)))
    [f3] (argless (if vemv/shortcuts/global/f3 (vemv/keyboard-funcall :vemv/shortcuts/global/f3 vemv/shortcuts/global/f3)))
    "<s-f3>" (argless (if vemv/shortcuts/global/S-f3 (vemv/keyboard-funcall :vemv/shortcuts/global/S-f3 vemv/shortcuts/global/S-f3)))
    [f4] (argless (if vemv/shortcuts/global/f4 (vemv/keyboard-funcall :vemv/shortcuts/global/f4 vemv/shortcuts/global/f4)))
    "<s-f4>" (argless (if vemv/shortcuts/global/S-f4 (vemv/keyboard-funcall :vemv/shortcuts/global/S-f4 vemv/shortcuts/global/S-f4)))
    [f5] (argless (if vemv/shortcuts/global/f5 (vemv/keyboard-funcall :vemv/shortcuts/global/f5 vemv/shortcuts/global/f5)))
    "<s-f5>" (argless (if vemv/shortcuts/global/S-f5 (vemv/keyboard-funcall :vemv/shortcuts/global/S-f5 vemv/shortcuts/global/S-f5)))
    [f6] (argless (if vemv/shortcuts/global/f6 (vemv/keyboard-funcall :vemv/shortcuts/global/f6 vemv/shortcuts/global/f6)))
    "<s-f6>" (argless (if vemv/shortcuts/global/S-f6 (vemv/keyboard-funcall :vemv/shortcuts/global/S-f6 vemv/shortcuts/global/S-f6)))
    [f7] (argless (if vemv/shortcuts/global/f7 (vemv/keyboard-funcall :vemv/shortcuts/global/f7 vemv/shortcuts/global/f7)))
    "<s-f7>" (argless (if vemv/shortcuts/global/S-f7 (vemv/keyboard-funcall :vemv/shortcuts/global/S-f7 vemv/shortcuts/global/S-f7)))
    [f8] (argless (if vemv/shortcuts/global/f8 (vemv/keyboard-funcall :vemv/shortcuts/global/f8 vemv/shortcuts/global/f8)))
    "<s-f8>" (argless (if vemv/shortcuts/global/S-f8 (vemv/keyboard-funcall :vemv/shortcuts/global/S-f8 vemv/shortcuts/global/S-f8)))
    [f9] (argless (if vemv/shortcuts/global/f9 (vemv/keyboard-funcall :vemv/shortcuts/global/f9 vemv/shortcuts/global/f9)))
    "<s-f9>" (argless (if vemv/shortcuts/global/S-f9 (vemv/keyboard-funcall :vemv/shortcuts/global/S-f9 vemv/shortcuts/global/S-f9)))
    "<backspace>" (argless (if vemv/shortcuts/global/backspace (vemv/keyboard-funcall :vemv/shortcuts/global/backspace vemv/shortcuts/global/backspace)))
    "S-<backspace>" (argless (if vemv/shortcuts/global/S-backspace (vemv/keyboard-funcall :vemv/shortcuts/global/S-backspace vemv/shortcuts/global/S-backspace)))
    "<down>" (argless (if vemv/shortcuts/global/down (vemv/keyboard-funcall :vemv/shortcuts/global/down vemv/shortcuts/global/down)))
    "S-<down>" (argless (if vemv/shortcuts/global/S-down (vemv/keyboard-funcall :vemv/shortcuts/global/S-down vemv/shortcuts/global/S-down)))
    "<end>" (argless (if vemv/shortcuts/global/end (vemv/keyboard-funcall :vemv/shortcuts/global/end vemv/shortcuts/global/end)))
    "S-<end>" (argless (if vemv/shortcuts/global/S-end (vemv/keyboard-funcall :vemv/shortcuts/global/S-end vemv/shortcuts/global/S-end)))
    "<home>" (argless (if vemv/shortcuts/global/home (vemv/keyboard-funcall :vemv/shortcuts/global/home vemv/shortcuts/global/home)))
    "S-<home>" (argless (if vemv/shortcuts/global/S-home (vemv/keyboard-funcall :vemv/shortcuts/global/S-home vemv/shortcuts/global/S-home)))
    "<left>" (argless (if vemv/shortcuts/global/left (vemv/keyboard-funcall :vemv/shortcuts/global/left vemv/shortcuts/global/left)))
    "S-<left>" (argless (if vemv/shortcuts/global/S-left (vemv/keyboard-funcall :vemv/shortcuts/global/S-left vemv/shortcuts/global/S-left)))
    "<next>" (argless (if vemv/shortcuts/global/next (vemv/keyboard-funcall :vemv/shortcuts/global/next vemv/shortcuts/global/next)))
    "S-<next>" (argless (if vemv/shortcuts/global/S-next (vemv/keyboard-funcall :vemv/shortcuts/global/S-next vemv/shortcuts/global/S-next)))
    "<prior>" (argless (if vemv/shortcuts/global/prior (vemv/keyboard-funcall :vemv/shortcuts/global/prior vemv/shortcuts/global/prior)))
    "S-<prior>" (argless (if vemv/shortcuts/global/S-prior (vemv/keyboard-funcall :vemv/shortcuts/global/S-prior vemv/shortcuts/global/S-prior)))
    "<right>" (argless (if vemv/shortcuts/global/right (vemv/keyboard-funcall :vemv/shortcuts/global/right vemv/shortcuts/global/right)))
    "S-<right>" (argless (if vemv/shortcuts/global/S-right (vemv/keyboard-funcall :vemv/shortcuts/global/S-right vemv/shortcuts/global/S-right)))
    "<up>" (argless (if vemv/shortcuts/global/up (vemv/keyboard-funcall :vemv/shortcuts/global/up vemv/shortcuts/global/up)))
    "S-<up>" (argless (if vemv/shortcuts/global/S-up (vemv/keyboard-funcall :vemv/shortcuts/global/S-up vemv/shortcuts/global/S-up)))
    "RET" (argless (if vemv/shortcuts/global/RET (vemv/keyboard-funcall :vemv/shortcuts/global/RET vemv/shortcuts/global/RET)))
    "S-RET" (argless (if vemv/shortcuts/global/S-RET (vemv/keyboard-funcall :vemv/shortcuts/global/S-RET vemv/shortcuts/global/S-RET)))
    "S-SPC" (argless (if vemv/shortcuts/global/S-SPC (vemv/keyboard-funcall :vemv/shortcuts/global/S-SPC vemv/shortcuts/global/S-SPC)))
    "C-a" (argless (if vemv/shortcuts/global/primary-a (vemv/keyboard-funcall :vemv/shortcuts/global/primary-a vemv/shortcuts/global/primary-a)))
    "C-M-a" (argless (if vemv/shortcuts/global/primary-secondary-a (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-a vemv/shortcuts/global/primary-secondary-a)))
    "C-S-a" (argless (if vemv/shortcuts/global/primary-S-a (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-a vemv/shortcuts/global/primary-S-a)))
    "C-b" (argless (if vemv/shortcuts/global/primary-b (vemv/keyboard-funcall :vemv/shortcuts/global/primary-b vemv/shortcuts/global/primary-b)))
    "C-M-b" (argless (if vemv/shortcuts/global/primary-secondary-b (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-b vemv/shortcuts/global/primary-secondary-b)))
    "C-S-b" (argless (if vemv/shortcuts/global/primary-S-b (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-b vemv/shortcuts/global/primary-S-b)))
    "C-c" (argless (if vemv/shortcuts/global/primary-c (vemv/keyboard-funcall :vemv/shortcuts/global/primary-c vemv/shortcuts/global/primary-c)))
    "C-M-c" (argless (if vemv/shortcuts/global/primary-secondary-c (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-c vemv/shortcuts/global/primary-secondary-c)))
    "C-S-c" (argless (if vemv/shortcuts/global/primary-S-c (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-c vemv/shortcuts/global/primary-S-c)))
    "C-d" (argless (if vemv/shortcuts/global/primary-d (vemv/keyboard-funcall :vemv/shortcuts/global/primary-d vemv/shortcuts/global/primary-d)))
    "C-M-d" (argless (if vemv/shortcuts/global/primary-secondary-d (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-d vemv/shortcuts/global/primary-secondary-d)))
    "C-S-d" (argless (if vemv/shortcuts/global/primary-S-d (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-d vemv/shortcuts/global/primary-S-d)))
    "C-e" (argless (if vemv/shortcuts/global/primary-e (vemv/keyboard-funcall :vemv/shortcuts/global/primary-e vemv/shortcuts/global/primary-e)))
    "C-M-e" (argless (if vemv/shortcuts/global/primary-secondary-e (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-e vemv/shortcuts/global/primary-secondary-e)))
    "C-S-e" (argless (if vemv/shortcuts/global/primary-S-e (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-e vemv/shortcuts/global/primary-S-e)))
    "C-f" (argless (if vemv/shortcuts/global/primary-f (vemv/keyboard-funcall :vemv/shortcuts/global/primary-f vemv/shortcuts/global/primary-f)))
    "C-M-f" (argless (if vemv/shortcuts/global/primary-secondary-f (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-f vemv/shortcuts/global/primary-secondary-f)))
    "C-S-f" (argless (if vemv/shortcuts/global/primary-S-f (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-f vemv/shortcuts/global/primary-S-f)))
    "C-h" (argless (if vemv/shortcuts/global/primary-h (vemv/keyboard-funcall :vemv/shortcuts/global/primary-h vemv/shortcuts/global/primary-h)))
    "C-M-h" (argless (if vemv/shortcuts/global/primary-secondary-h (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-h vemv/shortcuts/global/primary-secondary-h)))
    "C-S-h" (argless (if vemv/shortcuts/global/primary-S-h (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-h vemv/shortcuts/global/primary-S-h)))
    "C-j" (argless (if vemv/shortcuts/global/primary-j (vemv/keyboard-funcall :vemv/shortcuts/global/primary-j vemv/shortcuts/global/primary-j)))
    "C-M-j" (argless (if vemv/shortcuts/global/primary-secondary-j (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-j vemv/shortcuts/global/primary-secondary-j)))
    "C-S-j" (argless (if vemv/shortcuts/global/primary-S-j (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-j vemv/shortcuts/global/primary-S-j)))
    "C-k" (argless (if vemv/shortcuts/global/primary-k (vemv/keyboard-funcall :vemv/shortcuts/global/primary-k vemv/shortcuts/global/primary-k)))
    "C-M-k" (argless (if vemv/shortcuts/global/primary-secondary-k (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-k vemv/shortcuts/global/primary-secondary-k)))
    "C-S-k" (argless (if vemv/shortcuts/global/primary-S-k (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-k vemv/shortcuts/global/primary-S-k)))
    "C-l" (argless (if vemv/shortcuts/global/primary-l (vemv/keyboard-funcall :vemv/shortcuts/global/primary-l vemv/shortcuts/global/primary-l)))
    "C-M-l" (argless (if vemv/shortcuts/global/primary-secondary-l (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-l vemv/shortcuts/global/primary-secondary-l)))
    "C-S-l" (argless (if vemv/shortcuts/global/primary-S-l (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-l vemv/shortcuts/global/primary-S-l)))
    "C-n" (argless (if vemv/shortcuts/global/primary-n (vemv/keyboard-funcall :vemv/shortcuts/global/primary-n vemv/shortcuts/global/primary-n)))
    "C-M-n" (argless (if vemv/shortcuts/global/primary-secondary-n (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-n vemv/shortcuts/global/primary-secondary-n)))
    "C-S-n" (argless (if vemv/shortcuts/global/primary-S-n (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-n vemv/shortcuts/global/primary-S-n)))
    "C-o" (argless (if vemv/shortcuts/global/primary-o (vemv/keyboard-funcall :vemv/shortcuts/global/primary-o vemv/shortcuts/global/primary-o)))
    "C-M-o" (argless (if vemv/shortcuts/global/primary-secondary-o (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-o vemv/shortcuts/global/primary-secondary-o)))
    "C-S-o" (argless (if vemv/shortcuts/global/primary-S-o (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-o vemv/shortcuts/global/primary-S-o)))
    "C-p" (argless (if vemv/shortcuts/global/primary-p (vemv/keyboard-funcall :vemv/shortcuts/global/primary-p vemv/shortcuts/global/primary-p)))
    "C-M-p" (argless (if vemv/shortcuts/global/primary-secondary-p (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-p vemv/shortcuts/global/primary-secondary-p)))
    "C-S-p" (argless (if vemv/shortcuts/global/primary-S-p (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-p vemv/shortcuts/global/primary-S-p)))
    "C-q" (argless (if vemv/shortcuts/global/primary-q (vemv/keyboard-funcall :vemv/shortcuts/global/primary-q vemv/shortcuts/global/primary-q)))
    "C-M-q" (argless (if vemv/shortcuts/global/primary-secondary-q (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-q vemv/shortcuts/global/primary-secondary-q)))
    "C-S-q" (argless (if vemv/shortcuts/global/primary-S-q (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-q vemv/shortcuts/global/primary-S-q)))
    "C-r" (argless (if vemv/shortcuts/global/primary-r (vemv/keyboard-funcall :vemv/shortcuts/global/primary-r vemv/shortcuts/global/primary-r)))
    "C-M-r" (argless (if vemv/shortcuts/global/primary-secondary-r (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-r vemv/shortcuts/global/primary-secondary-r)))
    "C-S-r" (argless (if vemv/shortcuts/global/primary-S-r (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-r vemv/shortcuts/global/primary-S-r)))
    "C-s" (argless (if vemv/shortcuts/global/primary-s (vemv/keyboard-funcall :vemv/shortcuts/global/primary-s vemv/shortcuts/global/primary-s)))
    "C-M-s" (argless (if vemv/shortcuts/global/primary-secondary-s (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-s vemv/shortcuts/global/primary-secondary-s)))
    "C-S-s" (argless (if vemv/shortcuts/global/primary-S-s (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-s vemv/shortcuts/global/primary-S-s)))
    "C-t" (argless (if vemv/shortcuts/global/primary-t (vemv/keyboard-funcall :vemv/shortcuts/global/primary-t vemv/shortcuts/global/primary-t)))
    "C-M-t" (argless (if vemv/shortcuts/global/primary-secondary-t (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-t vemv/shortcuts/global/primary-secondary-t)))
    "C-S-t" (argless (if vemv/shortcuts/global/primary-S-t (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-t vemv/shortcuts/global/primary-S-t)))
    "C-u" (argless (if vemv/shortcuts/global/primary-u (vemv/keyboard-funcall :vemv/shortcuts/global/primary-u vemv/shortcuts/global/primary-u)))
    "C-M-u" (argless (if vemv/shortcuts/global/primary-secondary-u (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-u vemv/shortcuts/global/primary-secondary-u)))
    "C-S-u" (argless (if vemv/shortcuts/global/primary-S-u (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-u vemv/shortcuts/global/primary-S-u)))
    "C-v" (argless (if vemv/shortcuts/global/primary-v (vemv/keyboard-funcall :vemv/shortcuts/global/primary-v vemv/shortcuts/global/primary-v)))
    "C-M-v" (argless (if vemv/shortcuts/global/primary-secondary-v (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-v vemv/shortcuts/global/primary-secondary-v)))
    "C-S-v" (argless (if vemv/shortcuts/global/primary-S-v (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-v vemv/shortcuts/global/primary-S-v)))
    "C-w" (argless (if vemv/shortcuts/global/primary-w (vemv/keyboard-funcall :vemv/shortcuts/global/primary-w vemv/shortcuts/global/primary-w)))
    "C-M-w" (argless (if vemv/shortcuts/global/primary-secondary-w (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-w vemv/shortcuts/global/primary-secondary-w)))
    "C-S-w" (argless (if vemv/shortcuts/global/primary-S-w (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-w vemv/shortcuts/global/primary-S-w)))
    "C-x" (argless (if vemv/shortcuts/global/primary-x (vemv/keyboard-funcall :vemv/shortcuts/global/primary-x vemv/shortcuts/global/primary-x)))
    "C-M-x" (argless (if vemv/shortcuts/global/primary-secondary-x (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-x vemv/shortcuts/global/primary-secondary-x)))
    "C-S-x" (argless (if vemv/shortcuts/global/primary-S-x (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-x vemv/shortcuts/global/primary-S-x)))
    "C-y" (argless (if vemv/shortcuts/global/primary-y (vemv/keyboard-funcall :vemv/shortcuts/global/primary-y vemv/shortcuts/global/primary-y)))
    "C-M-y" (argless (if vemv/shortcuts/global/primary-secondary-y (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-y vemv/shortcuts/global/primary-secondary-y)))
    "C-S-y" (argless (if vemv/shortcuts/global/primary-S-y (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-y vemv/shortcuts/global/primary-S-y)))
    "C-z" (argless (if vemv/shortcuts/global/primary-z (vemv/keyboard-funcall :vemv/shortcuts/global/primary-z vemv/shortcuts/global/primary-z)))
    "C-M-z" (argless (if vemv/shortcuts/global/primary-secondary-z (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-z vemv/shortcuts/global/primary-secondary-z)))
    "C-S-z" (argless (if vemv/shortcuts/global/primary-S-z (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-z vemv/shortcuts/global/primary-S-z)))
    "C-1" (argless (if vemv/shortcuts/global/primary-1 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-1 vemv/shortcuts/global/primary-1)))
    "C-M-1" (argless (if vemv/shortcuts/global/primary-secondary-1 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-1 vemv/shortcuts/global/primary-secondary-1)))
    "C-S-1" (argless (if vemv/shortcuts/global/primary-S-1 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-1 vemv/shortcuts/global/primary-S-1)))
    "C-2" (argless (if vemv/shortcuts/global/primary-2 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-2 vemv/shortcuts/global/primary-2)))
    "C-M-2" (argless (if vemv/shortcuts/global/primary-secondary-2 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-2 vemv/shortcuts/global/primary-secondary-2)))
    "C-S-2" (argless (if vemv/shortcuts/global/primary-S-2 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-2 vemv/shortcuts/global/primary-S-2)))
    "C-3" (argless (if vemv/shortcuts/global/primary-3 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-3 vemv/shortcuts/global/primary-3)))
    "C-M-3" (argless (if vemv/shortcuts/global/primary-secondary-3 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-3 vemv/shortcuts/global/primary-secondary-3)))
    "C-S-3" (argless (if vemv/shortcuts/global/primary-S-3 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-3 vemv/shortcuts/global/primary-S-3)))
    "C-4" (argless (if vemv/shortcuts/global/primary-4 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-4 vemv/shortcuts/global/primary-4)))
    "C-M-4" (argless (if vemv/shortcuts/global/primary-secondary-4 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-4 vemv/shortcuts/global/primary-secondary-4)))
    "C-S-4" (argless (if vemv/shortcuts/global/primary-S-4 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-4 vemv/shortcuts/global/primary-S-4)))
    "C-5" (argless (if vemv/shortcuts/global/primary-5 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-5 vemv/shortcuts/global/primary-5)))
    "C-M-5" (argless (if vemv/shortcuts/global/primary-secondary-5 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-5 vemv/shortcuts/global/primary-secondary-5)))
    "C-S-5" (argless (if vemv/shortcuts/global/primary-S-5 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-5 vemv/shortcuts/global/primary-S-5)))
    "C-6" (argless (if vemv/shortcuts/global/primary-6 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-6 vemv/shortcuts/global/primary-6)))
    "C-M-6" (argless (if vemv/shortcuts/global/primary-secondary-6 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-6 vemv/shortcuts/global/primary-secondary-6)))
    "C-S-6" (argless (if vemv/shortcuts/global/primary-S-6 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-6 vemv/shortcuts/global/primary-S-6)))
    "C-7" (argless (if vemv/shortcuts/global/primary-7 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-7 vemv/shortcuts/global/primary-7)))
    "C-M-7" (argless (if vemv/shortcuts/global/primary-secondary-7 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-7 vemv/shortcuts/global/primary-secondary-7)))
    "C-S-7" (argless (if vemv/shortcuts/global/primary-S-7 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-7 vemv/shortcuts/global/primary-S-7)))
    "C-8" (argless (if vemv/shortcuts/global/primary-8 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-8 vemv/shortcuts/global/primary-8)))
    "C-M-8" (argless (if vemv/shortcuts/global/primary-secondary-8 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-8 vemv/shortcuts/global/primary-secondary-8)))
    "C-S-8" (argless (if vemv/shortcuts/global/primary-S-8 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-8 vemv/shortcuts/global/primary-S-8)))
    "C-9" (argless (if vemv/shortcuts/global/primary-9 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-9 vemv/shortcuts/global/primary-9)))
    "C-M-9" (argless (if vemv/shortcuts/global/primary-secondary-9 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-9 vemv/shortcuts/global/primary-secondary-9)))
    "C-S-9" (argless (if vemv/shortcuts/global/primary-S-9 (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-9 vemv/shortcuts/global/primary-S-9)))
    "C-!" (argless (if vemv/shortcuts/global/primary-bang (vemv/keyboard-funcall :vemv/shortcuts/global/primary-bang vemv/shortcuts/global/primary-bang)))
    "C-M-!" (argless (if vemv/shortcuts/global/primary-secondary-bang (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-bang vemv/shortcuts/global/primary-secondary-bang)))
    "C-S-!" (argless (if vemv/shortcuts/global/primary-S-bang (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-bang vemv/shortcuts/global/primary-S-bang)))
    "C-@" (argless (if vemv/shortcuts/global/primary-at (vemv/keyboard-funcall :vemv/shortcuts/global/primary-at vemv/shortcuts/global/primary-at)))
    "C-M-@" (argless (if vemv/shortcuts/global/primary-secondary-at (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-at vemv/shortcuts/global/primary-secondary-at)))
    "C-S-@" (argless (if vemv/shortcuts/global/primary-S-at (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-at vemv/shortcuts/global/primary-S-at)))
    "C-&" (argless (if vemv/shortcuts/global/primary-ampersand (vemv/keyboard-funcall :vemv/shortcuts/global/primary-ampersand vemv/shortcuts/global/primary-ampersand)))
    "C-M-&" (argless (if vemv/shortcuts/global/primary-secondary-ampersand (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-ampersand vemv/shortcuts/global/primary-secondary-ampersand)))
    "C-S-&" (argless (if vemv/shortcuts/global/primary-S-ampersand (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-ampersand vemv/shortcuts/global/primary-S-ampersand)))
    "C-#" (argless (if vemv/shortcuts/global/primary-hash (vemv/keyboard-funcall :vemv/shortcuts/global/primary-hash vemv/shortcuts/global/primary-hash)))
    "C-M-#" (argless (if vemv/shortcuts/global/primary-secondary-hash (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-hash vemv/shortcuts/global/primary-secondary-hash)))
    "C-S-#" (argless (if vemv/shortcuts/global/primary-S-hash (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-hash vemv/shortcuts/global/primary-S-hash)))
    "C-%" (argless (if vemv/shortcuts/global/primary-percent (vemv/keyboard-funcall :vemv/shortcuts/global/primary-percent vemv/shortcuts/global/primary-percent)))
    "C-M-%" (argless (if vemv/shortcuts/global/primary-secondary-percent (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-percent vemv/shortcuts/global/primary-secondary-percent)))
    "C-S-%" (argless (if vemv/shortcuts/global/primary-S-percent (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-percent vemv/shortcuts/global/primary-S-percent)))
    "C-^" (argless (if vemv/shortcuts/global/primary-caret (vemv/keyboard-funcall :vemv/shortcuts/global/primary-caret vemv/shortcuts/global/primary-caret)))
    "C-M-^" (argless (if vemv/shortcuts/global/primary-secondary-caret (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-caret vemv/shortcuts/global/primary-secondary-caret)))
    "C-S-^" (argless (if vemv/shortcuts/global/primary-S-caret (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-caret vemv/shortcuts/global/primary-S-caret)))
    "C-$" (argless (if vemv/shortcuts/global/primary-dollar (vemv/keyboard-funcall :vemv/shortcuts/global/primary-dollar vemv/shortcuts/global/primary-dollar)))
    "C-M-$" (argless (if vemv/shortcuts/global/primary-secondary-dollar (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-dollar vemv/shortcuts/global/primary-secondary-dollar)))
    "C-S-$" (argless (if vemv/shortcuts/global/primary-S-dollar (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-dollar vemv/shortcuts/global/primary-S-dollar)))
    "C-_" (argless (if vemv/shortcuts/global/primary-underscore (vemv/keyboard-funcall :vemv/shortcuts/global/primary-underscore vemv/shortcuts/global/primary-underscore)))
    "C-M-_" (argless (if vemv/shortcuts/global/primary-secondary-underscore (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-underscore vemv/shortcuts/global/primary-secondary-underscore)))
    "C-S-_" (argless (if vemv/shortcuts/global/primary-S-underscore (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-underscore vemv/shortcuts/global/primary-S-underscore)))
    "C--" (argless (if vemv/shortcuts/global/primary-dash (vemv/keyboard-funcall :vemv/shortcuts/global/primary-dash vemv/shortcuts/global/primary-dash)))
    "C-M--" (argless (if vemv/shortcuts/global/primary-secondary-dash (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-dash vemv/shortcuts/global/primary-secondary-dash)))
    "C-S--" (argless (if vemv/shortcuts/global/primary-S-dash (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-dash vemv/shortcuts/global/primary-S-dash)))
    "C-," (argless (if vemv/shortcuts/global/primary-comma (vemv/keyboard-funcall :vemv/shortcuts/global/primary-comma vemv/shortcuts/global/primary-comma)))
    "C-M-," (argless (if vemv/shortcuts/global/primary-secondary-comma (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-comma vemv/shortcuts/global/primary-secondary-comma)))
    "C-S-," (argless (if vemv/shortcuts/global/primary-S-comma (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-comma vemv/shortcuts/global/primary-S-comma)))
    "C-;" (argless (if vemv/shortcuts/global/primary-semicolon (vemv/keyboard-funcall :vemv/shortcuts/global/primary-semicolon vemv/shortcuts/global/primary-semicolon)))
    "C-M-;" (argless (if vemv/shortcuts/global/primary-secondary-semicolon (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-semicolon vemv/shortcuts/global/primary-secondary-semicolon)))
    "C-S-;" (argless (if vemv/shortcuts/global/primary-S-semicolon (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-semicolon vemv/shortcuts/global/primary-S-semicolon)))
    "C-:" (argless (if vemv/shortcuts/global/primary-colon (vemv/keyboard-funcall :vemv/shortcuts/global/primary-colon vemv/shortcuts/global/primary-colon)))
    "C-M-:" (argless (if vemv/shortcuts/global/primary-secondary-colon (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-colon vemv/shortcuts/global/primary-secondary-colon)))
    "C-S-:" (argless (if vemv/shortcuts/global/primary-S-colon (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-colon vemv/shortcuts/global/primary-S-colon)))
    "C-?" (argless (if vemv/shortcuts/global/primary-question-mark (vemv/keyboard-funcall :vemv/shortcuts/global/primary-question-mark vemv/shortcuts/global/primary-question-mark)))
    "C-M-?" (argless (if vemv/shortcuts/global/primary-secondary-question-mark (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-question-mark vemv/shortcuts/global/primary-secondary-question-mark)))
    "C-S-?" (argless (if vemv/shortcuts/global/primary-S-question-mark (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-question-mark vemv/shortcuts/global/primary-S-question-mark)))
    "C-." (argless (if vemv/shortcuts/global/primary-dot (vemv/keyboard-funcall :vemv/shortcuts/global/primary-dot vemv/shortcuts/global/primary-dot)))
    "C-M-." (argless (if vemv/shortcuts/global/primary-secondary-dot (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-dot vemv/shortcuts/global/primary-secondary-dot)))
    "C-S-." (argless (if vemv/shortcuts/global/primary-S-dot (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-dot vemv/shortcuts/global/primary-S-dot)))
    "C-'" (argless (if vemv/shortcuts/global/primary-single-quote (vemv/keyboard-funcall :vemv/shortcuts/global/primary-single-quote vemv/shortcuts/global/primary-single-quote)))
    "C-M-'" (argless (if vemv/shortcuts/global/primary-secondary-single-quote (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-single-quote vemv/shortcuts/global/primary-secondary-single-quote)))
    "C-S-'" (argless (if vemv/shortcuts/global/primary-S-single-quote (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-single-quote vemv/shortcuts/global/primary-S-single-quote)))
    "C-(" (argless (if vemv/shortcuts/global/primary-left-parens (vemv/keyboard-funcall :vemv/shortcuts/global/primary-left-parens vemv/shortcuts/global/primary-left-parens)))
    "C-M-(" (argless (if vemv/shortcuts/global/primary-secondary-left-parens (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-left-parens vemv/shortcuts/global/primary-secondary-left-parens)))
    "C-S-(" (argless (if vemv/shortcuts/global/primary-S-left-parens (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-left-parens vemv/shortcuts/global/primary-S-left-parens)))
    "C-)" (argless (if vemv/shortcuts/global/primary-right-parens (vemv/keyboard-funcall :vemv/shortcuts/global/primary-right-parens vemv/shortcuts/global/primary-right-parens)))
    "C-M-)" (argless (if vemv/shortcuts/global/primary-secondary-right-parens (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-right-parens vemv/shortcuts/global/primary-secondary-right-parens)))
    "C-S-)" (argless (if vemv/shortcuts/global/primary-S-right-parens (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-right-parens vemv/shortcuts/global/primary-S-right-parens)))
    "C-]" (argless (if vemv/shortcuts/global/primary-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/global/primary-right-bracket vemv/shortcuts/global/primary-right-bracket)))
    "C-M-]" (argless (if vemv/shortcuts/global/primary-secondary-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-right-bracket vemv/shortcuts/global/primary-secondary-right-bracket)))
    "C-S-]" (argless (if vemv/shortcuts/global/primary-S-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-right-bracket vemv/shortcuts/global/primary-S-right-bracket)))
    "C-{" (argless (if vemv/shortcuts/global/primary-left-curly (vemv/keyboard-funcall :vemv/shortcuts/global/primary-left-curly vemv/shortcuts/global/primary-left-curly)))
    "C-M-{" (argless (if vemv/shortcuts/global/primary-secondary-left-curly (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-left-curly vemv/shortcuts/global/primary-secondary-left-curly)))
    "C-S-{" (argless (if vemv/shortcuts/global/primary-S-left-curly (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-left-curly vemv/shortcuts/global/primary-S-left-curly)))
    "C-}" (argless (if vemv/shortcuts/global/primary-right-curly (vemv/keyboard-funcall :vemv/shortcuts/global/primary-right-curly vemv/shortcuts/global/primary-right-curly)))
    "C-M-}" (argless (if vemv/shortcuts/global/primary-secondary-right-curly (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-right-curly vemv/shortcuts/global/primary-secondary-right-curly)))
    "C-S-}" (argless (if vemv/shortcuts/global/primary-S-right-curly (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-right-curly vemv/shortcuts/global/primary-S-right-curly)))
    "C-*" (argless (if vemv/shortcuts/global/primary-star (vemv/keyboard-funcall :vemv/shortcuts/global/primary-star vemv/shortcuts/global/primary-star)))
    "C-M-*" (argless (if vemv/shortcuts/global/primary-secondary-star (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-star vemv/shortcuts/global/primary-secondary-star)))
    "C-S-*" (argless (if vemv/shortcuts/global/primary-S-star (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-star vemv/shortcuts/global/primary-S-star)))
    "C-/" (argless (if vemv/shortcuts/global/primary-slash (vemv/keyboard-funcall :vemv/shortcuts/global/primary-slash vemv/shortcuts/global/primary-slash)))
    "C-M-/" (argless (if vemv/shortcuts/global/primary-secondary-slash (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-slash vemv/shortcuts/global/primary-secondary-slash)))
    "C-S-/" (argless (if vemv/shortcuts/global/primary-S-slash (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-slash vemv/shortcuts/global/primary-S-slash)))
    "C-`" (argless (if vemv/shortcuts/global/primary-backtick (vemv/keyboard-funcall :vemv/shortcuts/global/primary-backtick vemv/shortcuts/global/primary-backtick)))
    "C-M-`" (argless (if vemv/shortcuts/global/primary-secondary-backtick (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-backtick vemv/shortcuts/global/primary-secondary-backtick)))
    "C-S-`" (argless (if vemv/shortcuts/global/primary-S-backtick (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-backtick vemv/shortcuts/global/primary-S-backtick)))
    "C-+" (argless (if vemv/shortcuts/global/primary-plus (vemv/keyboard-funcall :vemv/shortcuts/global/primary-plus vemv/shortcuts/global/primary-plus)))
    "C-M-+" (argless (if vemv/shortcuts/global/primary-secondary-plus (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-plus vemv/shortcuts/global/primary-secondary-plus)))
    "C-S-+" (argless (if vemv/shortcuts/global/primary-S-plus (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-plus vemv/shortcuts/global/primary-S-plus)))
    "C-<backspace>" (argless (if vemv/shortcuts/global/primary-backspace (vemv/keyboard-funcall :vemv/shortcuts/global/primary-backspace vemv/shortcuts/global/primary-backspace)))
    "C-M-<backspace>" (argless (if vemv/shortcuts/global/primary-secondary-backspace (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-backspace vemv/shortcuts/global/primary-secondary-backspace)))
    "C-S-<backspace>" (argless (if vemv/shortcuts/global/primary-S-backspace (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-backspace vemv/shortcuts/global/primary-S-backspace)))
    "C-<down>" (argless (if vemv/shortcuts/global/primary-down (vemv/keyboard-funcall :vemv/shortcuts/global/primary-down vemv/shortcuts/global/primary-down)))
    "C-M-<down>" (argless (if vemv/shortcuts/global/primary-secondary-down (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-down vemv/shortcuts/global/primary-secondary-down)))
    "C-S-<down>" (argless (if vemv/shortcuts/global/primary-S-down (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-down vemv/shortcuts/global/primary-S-down)))
    "C-<end>" (argless (if vemv/shortcuts/global/primary-end (vemv/keyboard-funcall :vemv/shortcuts/global/primary-end vemv/shortcuts/global/primary-end)))
    "C-M-<end>" (argless (if vemv/shortcuts/global/primary-secondary-end (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-end vemv/shortcuts/global/primary-secondary-end)))
    "C-S-<end>" (argless (if vemv/shortcuts/global/primary-S-end (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-end vemv/shortcuts/global/primary-S-end)))
    "C-<home>" (argless (if vemv/shortcuts/global/primary-home (vemv/keyboard-funcall :vemv/shortcuts/global/primary-home vemv/shortcuts/global/primary-home)))
    "C-M-<home>" (argless (if vemv/shortcuts/global/primary-secondary-home (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-home vemv/shortcuts/global/primary-secondary-home)))
    "C-S-<home>" (argless (if vemv/shortcuts/global/primary-S-home (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-home vemv/shortcuts/global/primary-S-home)))
    "C-<left>" (argless (if vemv/shortcuts/global/primary-left (vemv/keyboard-funcall :vemv/shortcuts/global/primary-left vemv/shortcuts/global/primary-left)))
    "C-M-<left>" (argless (if vemv/shortcuts/global/primary-secondary-left (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-left vemv/shortcuts/global/primary-secondary-left)))
    "C-S-<left>" (argless (if vemv/shortcuts/global/primary-S-left (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-left vemv/shortcuts/global/primary-S-left)))
    "C-<next>" (argless (if vemv/shortcuts/global/primary-next (vemv/keyboard-funcall :vemv/shortcuts/global/primary-next vemv/shortcuts/global/primary-next)))
    "C-M-<next>" (argless (if vemv/shortcuts/global/primary-secondary-next (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-next vemv/shortcuts/global/primary-secondary-next)))
    "C-S-<next>" (argless (if vemv/shortcuts/global/primary-S-next (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-next vemv/shortcuts/global/primary-S-next)))
    "C-<prior>" (argless (if vemv/shortcuts/global/primary-prior (vemv/keyboard-funcall :vemv/shortcuts/global/primary-prior vemv/shortcuts/global/primary-prior)))
    "C-M-<prior>" (argless (if vemv/shortcuts/global/primary-secondary-prior (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-prior vemv/shortcuts/global/primary-secondary-prior)))
    "C-S-<prior>" (argless (if vemv/shortcuts/global/primary-S-prior (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-prior vemv/shortcuts/global/primary-S-prior)))
    "C-<right>" (argless (if vemv/shortcuts/global/primary-right (vemv/keyboard-funcall :vemv/shortcuts/global/primary-right vemv/shortcuts/global/primary-right)))
    "C-M-<right>" (argless (if vemv/shortcuts/global/primary-secondary-right (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-right vemv/shortcuts/global/primary-secondary-right)))
    "C-S-<right>" (argless (if vemv/shortcuts/global/primary-S-right (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-right vemv/shortcuts/global/primary-S-right)))
    "C-<up>" (argless (if vemv/shortcuts/global/primary-up (vemv/keyboard-funcall :vemv/shortcuts/global/primary-up vemv/shortcuts/global/primary-up)))
    "C-M-<up>" (argless (if vemv/shortcuts/global/primary-secondary-up (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-up vemv/shortcuts/global/primary-secondary-up)))
    "C-S-<up>" (argless (if vemv/shortcuts/global/primary-S-up (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-up vemv/shortcuts/global/primary-S-up)))
    "C-=" (argless (if vemv/shortcuts/global/primary-equal (vemv/keyboard-funcall :vemv/shortcuts/global/primary-equal vemv/shortcuts/global/primary-equal)))
    "C-M-=" (argless (if vemv/shortcuts/global/primary-secondary-equal (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-equal vemv/shortcuts/global/primary-secondary-equal)))
    "C-S-=" (argless (if vemv/shortcuts/global/primary-S-equal (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-equal vemv/shortcuts/global/primary-S-equal)))
    "C-|" (argless (if vemv/shortcuts/global/primary-bar (vemv/keyboard-funcall :vemv/shortcuts/global/primary-bar vemv/shortcuts/global/primary-bar)))
    "C-M-|" (argless (if vemv/shortcuts/global/primary-secondary-bar (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-bar vemv/shortcuts/global/primary-secondary-bar)))
    "C-S-|" (argless (if vemv/shortcuts/global/primary-S-bar (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-bar vemv/shortcuts/global/primary-S-bar)))
    "C-RET" (argless (if vemv/shortcuts/global/primary-RET (vemv/keyboard-funcall :vemv/shortcuts/global/primary-RET vemv/shortcuts/global/primary-RET)))
    "C-M-RET" (argless (if vemv/shortcuts/global/primary-secondary-RET (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-RET vemv/shortcuts/global/primary-secondary-RET)))
    "C-S-RET" (argless (if vemv/shortcuts/global/primary-S-RET (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-RET vemv/shortcuts/global/primary-S-RET)))
    "C-SPC" (argless (if vemv/shortcuts/global/primary-SPC (vemv/keyboard-funcall :vemv/shortcuts/global/primary-SPC vemv/shortcuts/global/primary-SPC)))
    "C-M-SPC" (argless (if vemv/shortcuts/global/primary-secondary-SPC (vemv/keyboard-funcall :vemv/shortcuts/global/primary-secondary-SPC vemv/shortcuts/global/primary-secondary-SPC)))
    "C-S-SPC" (argless (if vemv/shortcuts/global/primary-S-SPC (vemv/keyboard-funcall :vemv/shortcuts/global/primary-S-SPC vemv/shortcuts/global/primary-S-SPC)))
    "M-a" (argless (if vemv/shortcuts/global/secondary-a (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-a vemv/shortcuts/global/secondary-a)))
    "M-S-a" (argless (if vemv/shortcuts/global/secondary-S-a (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-a vemv/shortcuts/global/secondary-S-a)))
    "M-b" (argless (if vemv/shortcuts/global/secondary-b (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-b vemv/shortcuts/global/secondary-b)))
    "M-S-b" (argless (if vemv/shortcuts/global/secondary-S-b (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-b vemv/shortcuts/global/secondary-S-b)))
    "M-c" (argless (if vemv/shortcuts/global/secondary-c (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-c vemv/shortcuts/global/secondary-c)))
    "M-S-c" (argless (if vemv/shortcuts/global/secondary-S-c (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-c vemv/shortcuts/global/secondary-S-c)))
    "M-d" (argless (if vemv/shortcuts/global/secondary-d (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-d vemv/shortcuts/global/secondary-d)))
    "M-S-d" (argless (if vemv/shortcuts/global/secondary-S-d (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-d vemv/shortcuts/global/secondary-S-d)))
    "M-e" (argless (if vemv/shortcuts/global/secondary-e (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-e vemv/shortcuts/global/secondary-e)))
    "M-S-e" (argless (if vemv/shortcuts/global/secondary-S-e (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-e vemv/shortcuts/global/secondary-S-e)))
    "M-f" (argless (if vemv/shortcuts/global/secondary-f (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-f vemv/shortcuts/global/secondary-f)))
    "M-S-f" (argless (if vemv/shortcuts/global/secondary-S-f (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-f vemv/shortcuts/global/secondary-S-f)))
    "M-g" (argless (if vemv/shortcuts/global/secondary-g (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-g vemv/shortcuts/global/secondary-g)))
    "M-S-g" (argless (if vemv/shortcuts/global/secondary-S-g (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-g vemv/shortcuts/global/secondary-S-g)))
    "M-h" (argless (if vemv/shortcuts/global/secondary-h (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-h vemv/shortcuts/global/secondary-h)))
    "M-S-h" (argless (if vemv/shortcuts/global/secondary-S-h (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-h vemv/shortcuts/global/secondary-S-h)))
    "M-i" (argless (if vemv/shortcuts/global/secondary-i (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-i vemv/shortcuts/global/secondary-i)))
    "M-S-i" (argless (if vemv/shortcuts/global/secondary-S-i (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-i vemv/shortcuts/global/secondary-S-i)))
    "M-j" (argless (if vemv/shortcuts/global/secondary-j (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-j vemv/shortcuts/global/secondary-j)))
    "M-S-j" (argless (if vemv/shortcuts/global/secondary-S-j (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-j vemv/shortcuts/global/secondary-S-j)))
    "M-k" (argless (if vemv/shortcuts/global/secondary-k (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-k vemv/shortcuts/global/secondary-k)))
    "M-S-k" (argless (if vemv/shortcuts/global/secondary-S-k (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-k vemv/shortcuts/global/secondary-S-k)))
    "M-l" (argless (if vemv/shortcuts/global/secondary-l (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-l vemv/shortcuts/global/secondary-l)))
    "M-S-l" (argless (if vemv/shortcuts/global/secondary-S-l (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-l vemv/shortcuts/global/secondary-S-l)))
    "M-m" (argless (if vemv/shortcuts/global/secondary-m (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-m vemv/shortcuts/global/secondary-m)))
    "M-S-m" (argless (if vemv/shortcuts/global/secondary-S-m (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-m vemv/shortcuts/global/secondary-S-m)))
    "M-n" (argless (if vemv/shortcuts/global/secondary-n (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-n vemv/shortcuts/global/secondary-n)))
    "M-S-n" (argless (if vemv/shortcuts/global/secondary-S-n (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-n vemv/shortcuts/global/secondary-S-n)))
    "M-o" (argless (if vemv/shortcuts/global/secondary-o (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-o vemv/shortcuts/global/secondary-o)))
    "M-S-o" (argless (if vemv/shortcuts/global/secondary-S-o (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-o vemv/shortcuts/global/secondary-S-o)))
    "M-p" (argless (if vemv/shortcuts/global/secondary-p (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-p vemv/shortcuts/global/secondary-p)))
    "M-S-p" (argless (if vemv/shortcuts/global/secondary-S-p (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-p vemv/shortcuts/global/secondary-S-p)))
    "M-q" (argless (if vemv/shortcuts/global/secondary-q (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-q vemv/shortcuts/global/secondary-q)))
    "M-S-q" (argless (if vemv/shortcuts/global/secondary-S-q (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-q vemv/shortcuts/global/secondary-S-q)))
    "M-r" (argless (if vemv/shortcuts/global/secondary-r (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-r vemv/shortcuts/global/secondary-r)))
    "M-S-r" (argless (if vemv/shortcuts/global/secondary-S-r (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-r vemv/shortcuts/global/secondary-S-r)))
    "M-s" (argless (if vemv/shortcuts/global/secondary-s (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-s vemv/shortcuts/global/secondary-s)))
    "M-S-s" (argless (if vemv/shortcuts/global/secondary-S-s (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-s vemv/shortcuts/global/secondary-S-s)))
    "M-t" (argless (if vemv/shortcuts/global/secondary-t (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-t vemv/shortcuts/global/secondary-t)))
    "M-S-t" (argless (if vemv/shortcuts/global/secondary-S-t (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-t vemv/shortcuts/global/secondary-S-t)))
    "M-u" (argless (if vemv/shortcuts/global/secondary-u (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-u vemv/shortcuts/global/secondary-u)))
    "M-S-u" (argless (if vemv/shortcuts/global/secondary-S-u (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-u vemv/shortcuts/global/secondary-S-u)))
    "M-v" (argless (if vemv/shortcuts/global/secondary-v (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-v vemv/shortcuts/global/secondary-v)))
    "M-S-v" (argless (if vemv/shortcuts/global/secondary-S-v (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-v vemv/shortcuts/global/secondary-S-v)))
    "M-w" (argless (if vemv/shortcuts/global/secondary-w (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-w vemv/shortcuts/global/secondary-w)))
    "M-S-w" (argless (if vemv/shortcuts/global/secondary-S-w (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-w vemv/shortcuts/global/secondary-S-w)))
    "M-x" (argless (if vemv/shortcuts/global/secondary-x (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-x vemv/shortcuts/global/secondary-x)))
    "M-S-x" (argless (if vemv/shortcuts/global/secondary-S-x (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-x vemv/shortcuts/global/secondary-S-x)))
    "M-y" (argless (if vemv/shortcuts/global/secondary-y (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-y vemv/shortcuts/global/secondary-y)))
    "M-S-y" (argless (if vemv/shortcuts/global/secondary-S-y (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-y vemv/shortcuts/global/secondary-S-y)))
    "M-z" (argless (if vemv/shortcuts/global/secondary-z (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-z vemv/shortcuts/global/secondary-z)))
    "M-S-z" (argless (if vemv/shortcuts/global/secondary-S-z (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-z vemv/shortcuts/global/secondary-S-z)))
    "M-0" (argless (if vemv/shortcuts/global/secondary-0 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-0 vemv/shortcuts/global/secondary-0)))
    "M-S-0" (argless (if vemv/shortcuts/global/secondary-S-0 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-0 vemv/shortcuts/global/secondary-S-0)))
    "M-1" (argless (if vemv/shortcuts/global/secondary-1 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-1 vemv/shortcuts/global/secondary-1)))
    "M-S-1" (argless (if vemv/shortcuts/global/secondary-S-1 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-1 vemv/shortcuts/global/secondary-S-1)))
    "M-2" (argless (if vemv/shortcuts/global/secondary-2 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-2 vemv/shortcuts/global/secondary-2)))
    "M-S-2" (argless (if vemv/shortcuts/global/secondary-S-2 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-2 vemv/shortcuts/global/secondary-S-2)))
    "M-3" (argless (if vemv/shortcuts/global/secondary-3 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-3 vemv/shortcuts/global/secondary-3)))
    "M-S-3" (argless (if vemv/shortcuts/global/secondary-S-3 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-3 vemv/shortcuts/global/secondary-S-3)))
    "M-4" (argless (if vemv/shortcuts/global/secondary-4 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-4 vemv/shortcuts/global/secondary-4)))
    "M-S-4" (argless (if vemv/shortcuts/global/secondary-S-4 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-4 vemv/shortcuts/global/secondary-S-4)))
    "M-5" (argless (if vemv/shortcuts/global/secondary-5 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-5 vemv/shortcuts/global/secondary-5)))
    "M-S-5" (argless (if vemv/shortcuts/global/secondary-S-5 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-5 vemv/shortcuts/global/secondary-S-5)))
    "M-6" (argless (if vemv/shortcuts/global/secondary-6 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-6 vemv/shortcuts/global/secondary-6)))
    "M-S-6" (argless (if vemv/shortcuts/global/secondary-S-6 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-6 vemv/shortcuts/global/secondary-S-6)))
    "M-7" (argless (if vemv/shortcuts/global/secondary-7 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-7 vemv/shortcuts/global/secondary-7)))
    "M-S-7" (argless (if vemv/shortcuts/global/secondary-S-7 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-7 vemv/shortcuts/global/secondary-S-7)))
    "M-8" (argless (if vemv/shortcuts/global/secondary-8 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-8 vemv/shortcuts/global/secondary-8)))
    "M-S-8" (argless (if vemv/shortcuts/global/secondary-S-8 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-8 vemv/shortcuts/global/secondary-S-8)))
    "M-9" (argless (if vemv/shortcuts/global/secondary-9 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-9 vemv/shortcuts/global/secondary-9)))
    "M-S-9" (argless (if vemv/shortcuts/global/secondary-S-9 (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-9 vemv/shortcuts/global/secondary-S-9)))
    "M-!" (argless (if vemv/shortcuts/global/secondary-bang (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-bang vemv/shortcuts/global/secondary-bang)))
    "M-S-!" (argless (if vemv/shortcuts/global/secondary-S-bang (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-bang vemv/shortcuts/global/secondary-S-bang)))
    "M-@" (argless (if vemv/shortcuts/global/secondary-at (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-at vemv/shortcuts/global/secondary-at)))
    "M-S-@" (argless (if vemv/shortcuts/global/secondary-S-at (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-at vemv/shortcuts/global/secondary-S-at)))
    "M-&" (argless (if vemv/shortcuts/global/secondary-ampersand (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-ampersand vemv/shortcuts/global/secondary-ampersand)))
    "M-S-&" (argless (if vemv/shortcuts/global/secondary-S-ampersand (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-ampersand vemv/shortcuts/global/secondary-S-ampersand)))
    "M-#" (argless (if vemv/shortcuts/global/secondary-hash (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-hash vemv/shortcuts/global/secondary-hash)))
    "M-S-#" (argless (if vemv/shortcuts/global/secondary-S-hash (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-hash vemv/shortcuts/global/secondary-S-hash)))
    "M-%" (argless (if vemv/shortcuts/global/secondary-percent (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-percent vemv/shortcuts/global/secondary-percent)))
    "M-S-%" (argless (if vemv/shortcuts/global/secondary-S-percent (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-percent vemv/shortcuts/global/secondary-S-percent)))
    "M-^" (argless (if vemv/shortcuts/global/secondary-caret (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-caret vemv/shortcuts/global/secondary-caret)))
    "M-S-^" (argless (if vemv/shortcuts/global/secondary-S-caret (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-caret vemv/shortcuts/global/secondary-S-caret)))
    "M-~" (argless (if vemv/shortcuts/global/secondary-tilde (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-tilde vemv/shortcuts/global/secondary-tilde)))
    "M-S-~" (argless (if vemv/shortcuts/global/secondary-S-tilde (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-tilde vemv/shortcuts/global/secondary-S-tilde)))
    "M-$" (argless (if vemv/shortcuts/global/secondary-dollar (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-dollar vemv/shortcuts/global/secondary-dollar)))
    "M-S-$" (argless (if vemv/shortcuts/global/secondary-S-dollar (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-dollar vemv/shortcuts/global/secondary-S-dollar)))
    "M-_" (argless (if vemv/shortcuts/global/secondary-underscore (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-underscore vemv/shortcuts/global/secondary-underscore)))
    "M-S-_" (argless (if vemv/shortcuts/global/secondary-S-underscore (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-underscore vemv/shortcuts/global/secondary-S-underscore)))
    "M--" (argless (if vemv/shortcuts/global/secondary-dash (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-dash vemv/shortcuts/global/secondary-dash)))
    "M-S--" (argless (if vemv/shortcuts/global/secondary-S-dash (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-dash vemv/shortcuts/global/secondary-S-dash)))
    "M-," (argless (if vemv/shortcuts/global/secondary-comma (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-comma vemv/shortcuts/global/secondary-comma)))
    "M-S-," (argless (if vemv/shortcuts/global/secondary-S-comma (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-comma vemv/shortcuts/global/secondary-S-comma)))
    "M-;" (argless (if vemv/shortcuts/global/secondary-semicolon (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-semicolon vemv/shortcuts/global/secondary-semicolon)))
    "M-S-;" (argless (if vemv/shortcuts/global/secondary-S-semicolon (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-semicolon vemv/shortcuts/global/secondary-S-semicolon)))
    "M-:" (argless (if vemv/shortcuts/global/secondary-colon (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-colon vemv/shortcuts/global/secondary-colon)))
    "M-S-:" (argless (if vemv/shortcuts/global/secondary-S-colon (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-colon vemv/shortcuts/global/secondary-S-colon)))
    "M-?" (argless (if vemv/shortcuts/global/secondary-question-mark (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-question-mark vemv/shortcuts/global/secondary-question-mark)))
    "M-S-?" (argless (if vemv/shortcuts/global/secondary-S-question-mark (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-question-mark vemv/shortcuts/global/secondary-S-question-mark)))
    "M-." (argless (if vemv/shortcuts/global/secondary-dot (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-dot vemv/shortcuts/global/secondary-dot)))
    "M-S-." (argless (if vemv/shortcuts/global/secondary-S-dot (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-dot vemv/shortcuts/global/secondary-S-dot)))
    "M-'" (argless (if vemv/shortcuts/global/secondary-single-quote (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-single-quote vemv/shortcuts/global/secondary-single-quote)))
    "M-S-'" (argless (if vemv/shortcuts/global/secondary-S-single-quote (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-single-quote vemv/shortcuts/global/secondary-S-single-quote)))
    "M-(" (argless (if vemv/shortcuts/global/secondary-left-parens (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-left-parens vemv/shortcuts/global/secondary-left-parens)))
    "M-S-(" (argless (if vemv/shortcuts/global/secondary-S-left-parens (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-left-parens vemv/shortcuts/global/secondary-S-left-parens)))
    "M-)" (argless (if vemv/shortcuts/global/secondary-right-parens (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-right-parens vemv/shortcuts/global/secondary-right-parens)))
    "M-S-)" (argless (if vemv/shortcuts/global/secondary-S-right-parens (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-right-parens vemv/shortcuts/global/secondary-S-right-parens)))
    "M-[" (argless (if vemv/shortcuts/global/secondary-left-bracket (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-left-bracket vemv/shortcuts/global/secondary-left-bracket)))
    "M-S-[" (argless (if vemv/shortcuts/global/secondary-S-left-bracket (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-left-bracket vemv/shortcuts/global/secondary-S-left-bracket)))
    "M-]" (argless (if vemv/shortcuts/global/secondary-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-right-bracket vemv/shortcuts/global/secondary-right-bracket)))
    "M-S-]" (argless (if vemv/shortcuts/global/secondary-S-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-right-bracket vemv/shortcuts/global/secondary-S-right-bracket)))
    "M-{" (argless (if vemv/shortcuts/global/secondary-left-curly (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-left-curly vemv/shortcuts/global/secondary-left-curly)))
    "M-S-{" (argless (if vemv/shortcuts/global/secondary-S-left-curly (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-left-curly vemv/shortcuts/global/secondary-S-left-curly)))
    "M-}" (argless (if vemv/shortcuts/global/secondary-right-curly (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-right-curly vemv/shortcuts/global/secondary-right-curly)))
    "M-S-}" (argless (if vemv/shortcuts/global/secondary-S-right-curly (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-right-curly vemv/shortcuts/global/secondary-S-right-curly)))
    "M-*" (argless (if vemv/shortcuts/global/secondary-star (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-star vemv/shortcuts/global/secondary-star)))
    "M-S-*" (argless (if vemv/shortcuts/global/secondary-S-star (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-star vemv/shortcuts/global/secondary-S-star)))
    "M-/" (argless (if vemv/shortcuts/global/secondary-slash (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-slash vemv/shortcuts/global/secondary-slash)))
    "M-S-/" (argless (if vemv/shortcuts/global/secondary-S-slash (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-slash vemv/shortcuts/global/secondary-S-slash)))
    "M-`" (argless (if vemv/shortcuts/global/secondary-backtick (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-backtick vemv/shortcuts/global/secondary-backtick)))
    "M-S-`" (argless (if vemv/shortcuts/global/secondary-S-backtick (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-backtick vemv/shortcuts/global/secondary-S-backtick)))
    "M-+" (argless (if vemv/shortcuts/global/secondary-plus (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-plus vemv/shortcuts/global/secondary-plus)))
    "M-S-+" (argless (if vemv/shortcuts/global/secondary-S-plus (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-plus vemv/shortcuts/global/secondary-S-plus)))
    "M-<backspace>" (argless (if vemv/shortcuts/global/secondary-backspace (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-backspace vemv/shortcuts/global/secondary-backspace)))
    "M-S-<backspace>" (argless (if vemv/shortcuts/global/secondary-S-backspace (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-backspace vemv/shortcuts/global/secondary-S-backspace)))
    "M-<down>" (argless (if vemv/shortcuts/global/secondary-down (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-down vemv/shortcuts/global/secondary-down)))
    "M-S-<down>" (argless (if vemv/shortcuts/global/secondary-S-down (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-down vemv/shortcuts/global/secondary-S-down)))
    "M-<end>" (argless (if vemv/shortcuts/global/secondary-end (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-end vemv/shortcuts/global/secondary-end)))
    "M-S-<end>" (argless (if vemv/shortcuts/global/secondary-S-end (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-end vemv/shortcuts/global/secondary-S-end)))
    "M-<home>" (argless (if vemv/shortcuts/global/secondary-home (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-home vemv/shortcuts/global/secondary-home)))
    "M-S-<home>" (argless (if vemv/shortcuts/global/secondary-S-home (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-home vemv/shortcuts/global/secondary-S-home)))
    "M-<left>" (argless (if vemv/shortcuts/global/secondary-left (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-left vemv/shortcuts/global/secondary-left)))
    "M-S-<left>" (argless (if vemv/shortcuts/global/secondary-S-left (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-left vemv/shortcuts/global/secondary-S-left)))
    "M-<next>" (argless (if vemv/shortcuts/global/secondary-next (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-next vemv/shortcuts/global/secondary-next)))
    "M-S-<next>" (argless (if vemv/shortcuts/global/secondary-S-next (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-next vemv/shortcuts/global/secondary-S-next)))
    "M-<prior>" (argless (if vemv/shortcuts/global/secondary-prior (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-prior vemv/shortcuts/global/secondary-prior)))
    "M-S-<prior>" (argless (if vemv/shortcuts/global/secondary-S-prior (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-prior vemv/shortcuts/global/secondary-S-prior)))
    "M-<right>" (argless (if vemv/shortcuts/global/secondary-right (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-right vemv/shortcuts/global/secondary-right)))
    "M-S-<right>" (argless (if vemv/shortcuts/global/secondary-S-right (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-right vemv/shortcuts/global/secondary-S-right)))
    "M-<up>" (argless (if vemv/shortcuts/global/secondary-up (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-up vemv/shortcuts/global/secondary-up)))
    "M-S-<up>" (argless (if vemv/shortcuts/global/secondary-S-up (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-up vemv/shortcuts/global/secondary-S-up)))
    "M-=" (argless (if vemv/shortcuts/global/secondary-equal (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-equal vemv/shortcuts/global/secondary-equal)))
    "M-S-=" (argless (if vemv/shortcuts/global/secondary-S-equal (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-equal vemv/shortcuts/global/secondary-S-equal)))
    "M-|" (argless (if vemv/shortcuts/global/secondary-bar (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-bar vemv/shortcuts/global/secondary-bar)))
    "M-S-|" (argless (if vemv/shortcuts/global/secondary-S-bar (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-bar vemv/shortcuts/global/secondary-S-bar)))
    "M-RET" (argless (if vemv/shortcuts/global/secondary-RET (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-RET vemv/shortcuts/global/secondary-RET)))
    "M-S-RET" (argless (if vemv/shortcuts/global/secondary-S-RET (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-RET vemv/shortcuts/global/secondary-S-RET)))
    "M-SPC" (argless (if vemv/shortcuts/global/secondary-SPC (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-SPC vemv/shortcuts/global/secondary-SPC)))
    "M-S-SPC" (argless (if vemv/shortcuts/global/secondary-S-SPC (vemv/keyboard-funcall :vemv/shortcuts/global/secondary-S-SPC vemv/shortcuts/global/secondary-S-SPC)))
    "s-a" (argless (if vemv/shortcuts/global/tertiary-a (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-a vemv/shortcuts/global/tertiary-a)))
    "s-S-a" (argless (if vemv/shortcuts/global/tertiary-S-a (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-a vemv/shortcuts/global/tertiary-S-a)))
    "s-b" (argless (if vemv/shortcuts/global/tertiary-b (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-b vemv/shortcuts/global/tertiary-b)))
    "s-S-b" (argless (if vemv/shortcuts/global/tertiary-S-b (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-b vemv/shortcuts/global/tertiary-S-b)))
    "s-c" (argless (if vemv/shortcuts/global/tertiary-c (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-c vemv/shortcuts/global/tertiary-c)))
    "s-S-c" (argless (if vemv/shortcuts/global/tertiary-S-c (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-c vemv/shortcuts/global/tertiary-S-c)))
    "s-d" (argless (if vemv/shortcuts/global/tertiary-d (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-d vemv/shortcuts/global/tertiary-d)))
    "s-S-d" (argless (if vemv/shortcuts/global/tertiary-S-d (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-d vemv/shortcuts/global/tertiary-S-d)))
    "s-e" (argless (if vemv/shortcuts/global/tertiary-e (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-e vemv/shortcuts/global/tertiary-e)))
    "s-S-e" (argless (if vemv/shortcuts/global/tertiary-S-e (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-e vemv/shortcuts/global/tertiary-S-e)))
    "s-f" (argless (if vemv/shortcuts/global/tertiary-f (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-f vemv/shortcuts/global/tertiary-f)))
    "s-S-f" (argless (if vemv/shortcuts/global/tertiary-S-f (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-f vemv/shortcuts/global/tertiary-S-f)))
    "s-g" (argless (if vemv/shortcuts/global/tertiary-g (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-g vemv/shortcuts/global/tertiary-g)))
    "s-S-g" (argless (if vemv/shortcuts/global/tertiary-S-g (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-g vemv/shortcuts/global/tertiary-S-g)))
    "s-h" (argless (if vemv/shortcuts/global/tertiary-h (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-h vemv/shortcuts/global/tertiary-h)))
    "s-S-h" (argless (if vemv/shortcuts/global/tertiary-S-h (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-h vemv/shortcuts/global/tertiary-S-h)))
    "s-i" (argless (if vemv/shortcuts/global/tertiary-i (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-i vemv/shortcuts/global/tertiary-i)))
    "s-S-i" (argless (if vemv/shortcuts/global/tertiary-S-i (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-i vemv/shortcuts/global/tertiary-S-i)))
    "s-j" (argless (if vemv/shortcuts/global/tertiary-j (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-j vemv/shortcuts/global/tertiary-j)))
    "s-S-j" (argless (if vemv/shortcuts/global/tertiary-S-j (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-j vemv/shortcuts/global/tertiary-S-j)))
    "s-k" (argless (if vemv/shortcuts/global/tertiary-k (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-k vemv/shortcuts/global/tertiary-k)))
    "s-S-k" (argless (if vemv/shortcuts/global/tertiary-S-k (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-k vemv/shortcuts/global/tertiary-S-k)))
    "s-l" (argless (if vemv/shortcuts/global/tertiary-l (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-l vemv/shortcuts/global/tertiary-l)))
    "s-S-l" (argless (if vemv/shortcuts/global/tertiary-S-l (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-l vemv/shortcuts/global/tertiary-S-l)))
    "s-m" (argless (if vemv/shortcuts/global/tertiary-m (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-m vemv/shortcuts/global/tertiary-m)))
    "s-S-m" (argless (if vemv/shortcuts/global/tertiary-S-m (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-m vemv/shortcuts/global/tertiary-S-m)))
    "s-n" (argless (if vemv/shortcuts/global/tertiary-n (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-n vemv/shortcuts/global/tertiary-n)))
    "s-S-n" (argless (if vemv/shortcuts/global/tertiary-S-n (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-n vemv/shortcuts/global/tertiary-S-n)))
    "s-o" (argless (if vemv/shortcuts/global/tertiary-o (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-o vemv/shortcuts/global/tertiary-o)))
    "s-S-o" (argless (if vemv/shortcuts/global/tertiary-S-o (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-o vemv/shortcuts/global/tertiary-S-o)))
    "s-p" (argless (if vemv/shortcuts/global/tertiary-p (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-p vemv/shortcuts/global/tertiary-p)))
    "s-S-p" (argless (if vemv/shortcuts/global/tertiary-S-p (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-p vemv/shortcuts/global/tertiary-S-p)))
    "s-q" (argless (if vemv/shortcuts/global/tertiary-q (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-q vemv/shortcuts/global/tertiary-q)))
    "s-S-q" (argless (if vemv/shortcuts/global/tertiary-S-q (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-q vemv/shortcuts/global/tertiary-S-q)))
    "s-r" (argless (if vemv/shortcuts/global/tertiary-r (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-r vemv/shortcuts/global/tertiary-r)))
    "s-S-r" (argless (if vemv/shortcuts/global/tertiary-S-r (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-r vemv/shortcuts/global/tertiary-S-r)))
    "s-s" (argless (if vemv/shortcuts/global/tertiary-s (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-s vemv/shortcuts/global/tertiary-s)))
    "s-S-s" (argless (if vemv/shortcuts/global/tertiary-S-s (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-s vemv/shortcuts/global/tertiary-S-s)))
    "s-t" (argless (if vemv/shortcuts/global/tertiary-t (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-t vemv/shortcuts/global/tertiary-t)))
    "s-S-t" (argless (if vemv/shortcuts/global/tertiary-S-t (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-t vemv/shortcuts/global/tertiary-S-t)))
    "s-u" (argless (if vemv/shortcuts/global/tertiary-u (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-u vemv/shortcuts/global/tertiary-u)))
    "s-S-u" (argless (if vemv/shortcuts/global/tertiary-S-u (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-u vemv/shortcuts/global/tertiary-S-u)))
    "s-v" (argless (if vemv/shortcuts/global/tertiary-v (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-v vemv/shortcuts/global/tertiary-v)))
    "s-S-v" (argless (if vemv/shortcuts/global/tertiary-S-v (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-v vemv/shortcuts/global/tertiary-S-v)))
    "s-w" (argless (if vemv/shortcuts/global/tertiary-w (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-w vemv/shortcuts/global/tertiary-w)))
    "s-S-w" (argless (if vemv/shortcuts/global/tertiary-S-w (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-w vemv/shortcuts/global/tertiary-S-w)))
    "s-x" (argless (if vemv/shortcuts/global/tertiary-x (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-x vemv/shortcuts/global/tertiary-x)))
    "s-S-x" (argless (if vemv/shortcuts/global/tertiary-S-x (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-x vemv/shortcuts/global/tertiary-S-x)))
    "s-y" (argless (if vemv/shortcuts/global/tertiary-y (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-y vemv/shortcuts/global/tertiary-y)))
    "s-S-y" (argless (if vemv/shortcuts/global/tertiary-S-y (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-y vemv/shortcuts/global/tertiary-S-y)))
    "s-z" (argless (if vemv/shortcuts/global/tertiary-z (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-z vemv/shortcuts/global/tertiary-z)))
    "s-S-z" (argless (if vemv/shortcuts/global/tertiary-S-z (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-z vemv/shortcuts/global/tertiary-S-z)))
    "s-0" (argless (if vemv/shortcuts/global/tertiary-0 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-0 vemv/shortcuts/global/tertiary-0)))
    "s-S-0" (argless (if vemv/shortcuts/global/tertiary-S-0 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-0 vemv/shortcuts/global/tertiary-S-0)))
    "s-1" (argless (if vemv/shortcuts/global/tertiary-1 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-1 vemv/shortcuts/global/tertiary-1)))
    "s-S-1" (argless (if vemv/shortcuts/global/tertiary-S-1 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-1 vemv/shortcuts/global/tertiary-S-1)))
    "s-2" (argless (if vemv/shortcuts/global/tertiary-2 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-2 vemv/shortcuts/global/tertiary-2)))
    "s-S-2" (argless (if vemv/shortcuts/global/tertiary-S-2 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-2 vemv/shortcuts/global/tertiary-S-2)))
    "s-3" (argless (if vemv/shortcuts/global/tertiary-3 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-3 vemv/shortcuts/global/tertiary-3)))
    "s-S-3" (argless (if vemv/shortcuts/global/tertiary-S-3 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-3 vemv/shortcuts/global/tertiary-S-3)))
    "s-4" (argless (if vemv/shortcuts/global/tertiary-4 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-4 vemv/shortcuts/global/tertiary-4)))
    "s-S-4" (argless (if vemv/shortcuts/global/tertiary-S-4 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-4 vemv/shortcuts/global/tertiary-S-4)))
    "s-5" (argless (if vemv/shortcuts/global/tertiary-5 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-5 vemv/shortcuts/global/tertiary-5)))
    "s-S-5" (argless (if vemv/shortcuts/global/tertiary-S-5 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-5 vemv/shortcuts/global/tertiary-S-5)))
    "s-6" (argless (if vemv/shortcuts/global/tertiary-6 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-6 vemv/shortcuts/global/tertiary-6)))
    "s-S-6" (argless (if vemv/shortcuts/global/tertiary-S-6 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-6 vemv/shortcuts/global/tertiary-S-6)))
    "s-7" (argless (if vemv/shortcuts/global/tertiary-7 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-7 vemv/shortcuts/global/tertiary-7)))
    "s-S-7" (argless (if vemv/shortcuts/global/tertiary-S-7 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-7 vemv/shortcuts/global/tertiary-S-7)))
    "s-8" (argless (if vemv/shortcuts/global/tertiary-8 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-8 vemv/shortcuts/global/tertiary-8)))
    "s-S-8" (argless (if vemv/shortcuts/global/tertiary-S-8 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-8 vemv/shortcuts/global/tertiary-S-8)))
    "s-9" (argless (if vemv/shortcuts/global/tertiary-9 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-9 vemv/shortcuts/global/tertiary-9)))
    "s-S-9" (argless (if vemv/shortcuts/global/tertiary-S-9 (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-9 vemv/shortcuts/global/tertiary-S-9)))
    "s-!" (argless (if vemv/shortcuts/global/tertiary-bang (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-bang vemv/shortcuts/global/tertiary-bang)))
    "s-S-!" (argless (if vemv/shortcuts/global/tertiary-S-bang (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-bang vemv/shortcuts/global/tertiary-S-bang)))
    "s-@" (argless (if vemv/shortcuts/global/tertiary-at (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-at vemv/shortcuts/global/tertiary-at)))
    "s-S-@" (argless (if vemv/shortcuts/global/tertiary-S-at (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-at vemv/shortcuts/global/tertiary-S-at)))
    "s-&" (argless (if vemv/shortcuts/global/tertiary-ampersand (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-ampersand vemv/shortcuts/global/tertiary-ampersand)))
    "s-S-&" (argless (if vemv/shortcuts/global/tertiary-S-ampersand (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-ampersand vemv/shortcuts/global/tertiary-S-ampersand)))
    "s-#" (argless (if vemv/shortcuts/global/tertiary-hash (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-hash vemv/shortcuts/global/tertiary-hash)))
    "s-S-#" (argless (if vemv/shortcuts/global/tertiary-S-hash (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-hash vemv/shortcuts/global/tertiary-S-hash)))
    "s-%" (argless (if vemv/shortcuts/global/tertiary-percent (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-percent vemv/shortcuts/global/tertiary-percent)))
    "s-S-%" (argless (if vemv/shortcuts/global/tertiary-S-percent (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-percent vemv/shortcuts/global/tertiary-S-percent)))
    "s-^" (argless (if vemv/shortcuts/global/tertiary-caret (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-caret vemv/shortcuts/global/tertiary-caret)))
    "s-S-^" (argless (if vemv/shortcuts/global/tertiary-S-caret (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-caret vemv/shortcuts/global/tertiary-S-caret)))
    "s-~" (argless (if vemv/shortcuts/global/tertiary-tilde (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-tilde vemv/shortcuts/global/tertiary-tilde)))
    "s-S-~" (argless (if vemv/shortcuts/global/tertiary-S-tilde (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-tilde vemv/shortcuts/global/tertiary-S-tilde)))
    "s-$" (argless (if vemv/shortcuts/global/tertiary-dollar (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-dollar vemv/shortcuts/global/tertiary-dollar)))
    "s-S-$" (argless (if vemv/shortcuts/global/tertiary-S-dollar (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-dollar vemv/shortcuts/global/tertiary-S-dollar)))
    "s-_" (argless (if vemv/shortcuts/global/tertiary-underscore (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-underscore vemv/shortcuts/global/tertiary-underscore)))
    "s-S-_" (argless (if vemv/shortcuts/global/tertiary-S-underscore (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-underscore vemv/shortcuts/global/tertiary-S-underscore)))
    "s--" (argless (if vemv/shortcuts/global/tertiary-dash (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-dash vemv/shortcuts/global/tertiary-dash)))
    "s-S--" (argless (if vemv/shortcuts/global/tertiary-S-dash (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-dash vemv/shortcuts/global/tertiary-S-dash)))
    "s-," (argless (if vemv/shortcuts/global/tertiary-comma (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-comma vemv/shortcuts/global/tertiary-comma)))
    "s-S-," (argless (if vemv/shortcuts/global/tertiary-S-comma (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-comma vemv/shortcuts/global/tertiary-S-comma)))
    "s-;" (argless (if vemv/shortcuts/global/tertiary-semicolon (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-semicolon vemv/shortcuts/global/tertiary-semicolon)))
    "s-S-;" (argless (if vemv/shortcuts/global/tertiary-S-semicolon (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-semicolon vemv/shortcuts/global/tertiary-S-semicolon)))
    "s-:" (argless (if vemv/shortcuts/global/tertiary-colon (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-colon vemv/shortcuts/global/tertiary-colon)))
    "s-S-:" (argless (if vemv/shortcuts/global/tertiary-S-colon (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-colon vemv/shortcuts/global/tertiary-S-colon)))
    "s-?" (argless (if vemv/shortcuts/global/tertiary-question-mark (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-question-mark vemv/shortcuts/global/tertiary-question-mark)))
    "s-S-?" (argless (if vemv/shortcuts/global/tertiary-S-question-mark (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-question-mark vemv/shortcuts/global/tertiary-S-question-mark)))
    "s-." (argless (if vemv/shortcuts/global/tertiary-dot (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-dot vemv/shortcuts/global/tertiary-dot)))
    "s-S-." (argless (if vemv/shortcuts/global/tertiary-S-dot (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-dot vemv/shortcuts/global/tertiary-S-dot)))
    "s-'" (argless (if vemv/shortcuts/global/tertiary-single-quote (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-single-quote vemv/shortcuts/global/tertiary-single-quote)))
    "s-S-'" (argless (if vemv/shortcuts/global/tertiary-S-single-quote (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-single-quote vemv/shortcuts/global/tertiary-S-single-quote)))
    "s-(" (argless (if vemv/shortcuts/global/tertiary-left-parens (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-left-parens vemv/shortcuts/global/tertiary-left-parens)))
    "s-S-(" (argless (if vemv/shortcuts/global/tertiary-S-left-parens (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-left-parens vemv/shortcuts/global/tertiary-S-left-parens)))
    "s-)" (argless (if vemv/shortcuts/global/tertiary-right-parens (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-right-parens vemv/shortcuts/global/tertiary-right-parens)))
    "s-S-)" (argless (if vemv/shortcuts/global/tertiary-S-right-parens (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-right-parens vemv/shortcuts/global/tertiary-S-right-parens)))
    "s-[" (argless (if vemv/shortcuts/global/tertiary-left-bracket (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-left-bracket vemv/shortcuts/global/tertiary-left-bracket)))
    "s-S-[" (argless (if vemv/shortcuts/global/tertiary-S-left-bracket (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-left-bracket vemv/shortcuts/global/tertiary-S-left-bracket)))
    "s-]" (argless (if vemv/shortcuts/global/tertiary-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-right-bracket vemv/shortcuts/global/tertiary-right-bracket)))
    "s-S-]" (argless (if vemv/shortcuts/global/tertiary-S-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-right-bracket vemv/shortcuts/global/tertiary-S-right-bracket)))
    "s-{" (argless (if vemv/shortcuts/global/tertiary-left-curly (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-left-curly vemv/shortcuts/global/tertiary-left-curly)))
    "s-S-{" (argless (if vemv/shortcuts/global/tertiary-S-left-curly (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-left-curly vemv/shortcuts/global/tertiary-S-left-curly)))
    "s-}" (argless (if vemv/shortcuts/global/tertiary-right-curly (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-right-curly vemv/shortcuts/global/tertiary-right-curly)))
    "s-S-}" (argless (if vemv/shortcuts/global/tertiary-S-right-curly (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-right-curly vemv/shortcuts/global/tertiary-S-right-curly)))
    "s-*" (argless (if vemv/shortcuts/global/tertiary-star (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-star vemv/shortcuts/global/tertiary-star)))
    "s-S-*" (argless (if vemv/shortcuts/global/tertiary-S-star (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-star vemv/shortcuts/global/tertiary-S-star)))
    "s-/" (argless (if vemv/shortcuts/global/tertiary-slash (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-slash vemv/shortcuts/global/tertiary-slash)))
    "s-S-/" (argless (if vemv/shortcuts/global/tertiary-S-slash (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-slash vemv/shortcuts/global/tertiary-S-slash)))
    "s-`" (argless (if vemv/shortcuts/global/tertiary-backtick (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-backtick vemv/shortcuts/global/tertiary-backtick)))
    "s-S-`" (argless (if vemv/shortcuts/global/tertiary-S-backtick (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-backtick vemv/shortcuts/global/tertiary-S-backtick)))
    "s-+" (argless (if vemv/shortcuts/global/tertiary-plus (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-plus vemv/shortcuts/global/tertiary-plus)))
    "s-S-+" (argless (if vemv/shortcuts/global/tertiary-S-plus (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-plus vemv/shortcuts/global/tertiary-S-plus)))
    "s-<backspace>" (argless (if vemv/shortcuts/global/tertiary-backspace (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-backspace vemv/shortcuts/global/tertiary-backspace)))
    "s-S-<backspace>" (argless (if vemv/shortcuts/global/tertiary-S-backspace (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-backspace vemv/shortcuts/global/tertiary-S-backspace)))
    "s-<down>" (argless (if vemv/shortcuts/global/tertiary-down (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-down vemv/shortcuts/global/tertiary-down)))
    "s-S-<down>" (argless (if vemv/shortcuts/global/tertiary-S-down (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-down vemv/shortcuts/global/tertiary-S-down)))
    "s-<end>" (argless (if vemv/shortcuts/global/tertiary-end (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-end vemv/shortcuts/global/tertiary-end)))
    "s-S-<end>" (argless (if vemv/shortcuts/global/tertiary-S-end (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-end vemv/shortcuts/global/tertiary-S-end)))
    "s-<home>" (argless (if vemv/shortcuts/global/tertiary-home (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-home vemv/shortcuts/global/tertiary-home)))
    "s-S-<home>" (argless (if vemv/shortcuts/global/tertiary-S-home (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-home vemv/shortcuts/global/tertiary-S-home)))
    "s-<left>" (argless (if vemv/shortcuts/global/tertiary-left (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-left vemv/shortcuts/global/tertiary-left)))
    "s-S-<left>" (argless (if vemv/shortcuts/global/tertiary-S-left (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-left vemv/shortcuts/global/tertiary-S-left)))
    "s-<next>" (argless (if vemv/shortcuts/global/tertiary-next (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-next vemv/shortcuts/global/tertiary-next)))
    "s-S-<next>" (argless (if vemv/shortcuts/global/tertiary-S-next (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-next vemv/shortcuts/global/tertiary-S-next)))
    "s-<prior>" (argless (if vemv/shortcuts/global/tertiary-prior (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-prior vemv/shortcuts/global/tertiary-prior)))
    "s-S-<prior>" (argless (if vemv/shortcuts/global/tertiary-S-prior (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-prior vemv/shortcuts/global/tertiary-S-prior)))
    "s-<right>" (argless (if vemv/shortcuts/global/tertiary-right (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-right vemv/shortcuts/global/tertiary-right)))
    "s-S-<right>" (argless (if vemv/shortcuts/global/tertiary-S-right (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-right vemv/shortcuts/global/tertiary-S-right)))
    "s-<up>" (argless (if vemv/shortcuts/global/tertiary-up (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-up vemv/shortcuts/global/tertiary-up)))
    "s-S-<up>" (argless (if vemv/shortcuts/global/tertiary-S-up (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-up vemv/shortcuts/global/tertiary-S-up)))
    "s-=" (argless (if vemv/shortcuts/global/tertiary-equal (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-equal vemv/shortcuts/global/tertiary-equal)))
    "s-S-=" (argless (if vemv/shortcuts/global/tertiary-S-equal (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-equal vemv/shortcuts/global/tertiary-S-equal)))
    "s-|" (argless (if vemv/shortcuts/global/tertiary-bar (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-bar vemv/shortcuts/global/tertiary-bar)))
    "s-S-|" (argless (if vemv/shortcuts/global/tertiary-S-bar (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-bar vemv/shortcuts/global/tertiary-S-bar)))
    "s-RET" (argless (if vemv/shortcuts/global/tertiary-RET (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-RET vemv/shortcuts/global/tertiary-RET)))
    "s-S-RET" (argless (if vemv/shortcuts/global/tertiary-S-RET (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-RET vemv/shortcuts/global/tertiary-S-RET)))
    "s-SPC" (argless (if vemv/shortcuts/global/tertiary-SPC (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-SPC vemv/shortcuts/global/tertiary-SPC)))
    "s-S-SPC" (argless (if vemv/shortcuts/global/tertiary-S-SPC (vemv/keyboard-funcall :vemv/shortcuts/global/tertiary-S-SPC vemv/shortcuts/global/tertiary-S-SPC)))
))

(setq vemv/clojure-key-bindings
  (vemv/hash-map
    [f1] (argless (if vemv/shortcuts/clojure/f1 (vemv/keyboard-funcall :vemv/shortcuts/clojure/f1 vemv/shortcuts/clojure/f1)))
    "<s-f1>" (argless (if vemv/shortcuts/clojure/S-f1 (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-f1 vemv/shortcuts/clojure/S-f1)))
    [f10] (argless (if vemv/shortcuts/clojure/f10 (vemv/keyboard-funcall :vemv/shortcuts/clojure/f10 vemv/shortcuts/clojure/f10)))
    "<s-f10>" (argless (if vemv/shortcuts/clojure/S-f10 (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-f10 vemv/shortcuts/clojure/S-f10)))
    [f11] (argless (if vemv/shortcuts/clojure/f11 (vemv/keyboard-funcall :vemv/shortcuts/clojure/f11 vemv/shortcuts/clojure/f11)))
    "<s-f11>" (argless (if vemv/shortcuts/clojure/S-f11 (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-f11 vemv/shortcuts/clojure/S-f11)))
    [f12] (argless (if vemv/shortcuts/clojure/f12 (vemv/keyboard-funcall :vemv/shortcuts/clojure/f12 vemv/shortcuts/clojure/f12)))
    "<s-f12>" (argless (if vemv/shortcuts/clojure/S-f12 (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-f12 vemv/shortcuts/clojure/S-f12)))
    [f2] (argless (if vemv/shortcuts/clojure/f2 (vemv/keyboard-funcall :vemv/shortcuts/clojure/f2 vemv/shortcuts/clojure/f2)))
    "<s-f2>" (argless (if vemv/shortcuts/clojure/S-f2 (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-f2 vemv/shortcuts/clojure/S-f2)))
    [f3] (argless (if vemv/shortcuts/clojure/f3 (vemv/keyboard-funcall :vemv/shortcuts/clojure/f3 vemv/shortcuts/clojure/f3)))
    "<s-f3>" (argless (if vemv/shortcuts/clojure/S-f3 (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-f3 vemv/shortcuts/clojure/S-f3)))
    [f4] (argless (if vemv/shortcuts/clojure/f4 (vemv/keyboard-funcall :vemv/shortcuts/clojure/f4 vemv/shortcuts/clojure/f4)))
    "<s-f4>" (argless (if vemv/shortcuts/clojure/S-f4 (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-f4 vemv/shortcuts/clojure/S-f4)))
    [f5] (argless (if vemv/shortcuts/clojure/f5 (vemv/keyboard-funcall :vemv/shortcuts/clojure/f5 vemv/shortcuts/clojure/f5)))
    "<s-f5>" (argless (if vemv/shortcuts/clojure/S-f5 (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-f5 vemv/shortcuts/clojure/S-f5)))
    [f6] (argless (if vemv/shortcuts/clojure/f6 (vemv/keyboard-funcall :vemv/shortcuts/clojure/f6 vemv/shortcuts/clojure/f6)))
    "<s-f6>" (argless (if vemv/shortcuts/clojure/S-f6 (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-f6 vemv/shortcuts/clojure/S-f6)))
    [f7] (argless (if vemv/shortcuts/clojure/f7 (vemv/keyboard-funcall :vemv/shortcuts/clojure/f7 vemv/shortcuts/clojure/f7)))
    "<s-f7>" (argless (if vemv/shortcuts/clojure/S-f7 (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-f7 vemv/shortcuts/clojure/S-f7)))
    [f8] (argless (if vemv/shortcuts/clojure/f8 (vemv/keyboard-funcall :vemv/shortcuts/clojure/f8 vemv/shortcuts/clojure/f8)))
    "<s-f8>" (argless (if vemv/shortcuts/clojure/S-f8 (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-f8 vemv/shortcuts/clojure/S-f8)))
    [f9] (argless (if vemv/shortcuts/clojure/f9 (vemv/keyboard-funcall :vemv/shortcuts/clojure/f9 vemv/shortcuts/clojure/f9)))
    "<s-f9>" (argless (if vemv/shortcuts/clojure/S-f9 (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-f9 vemv/shortcuts/clojure/S-f9)))
    "<backspace>" (argless (if vemv/shortcuts/clojure/backspace (vemv/keyboard-funcall :vemv/shortcuts/clojure/backspace vemv/shortcuts/clojure/backspace)))
    "S-<backspace>" (argless (if vemv/shortcuts/clojure/S-backspace (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-backspace vemv/shortcuts/clojure/S-backspace)))
    "<down>" (argless (if vemv/shortcuts/clojure/down (vemv/keyboard-funcall :vemv/shortcuts/clojure/down vemv/shortcuts/clojure/down)))
    "S-<down>" (argless (if vemv/shortcuts/clojure/S-down (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-down vemv/shortcuts/clojure/S-down)))
    "<end>" (argless (if vemv/shortcuts/clojure/end (vemv/keyboard-funcall :vemv/shortcuts/clojure/end vemv/shortcuts/clojure/end)))
    "S-<end>" (argless (if vemv/shortcuts/clojure/S-end (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-end vemv/shortcuts/clojure/S-end)))
    "<home>" (argless (if vemv/shortcuts/clojure/home (vemv/keyboard-funcall :vemv/shortcuts/clojure/home vemv/shortcuts/clojure/home)))
    "S-<home>" (argless (if vemv/shortcuts/clojure/S-home (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-home vemv/shortcuts/clojure/S-home)))
    "<left>" (argless (if vemv/shortcuts/clojure/left (vemv/keyboard-funcall :vemv/shortcuts/clojure/left vemv/shortcuts/clojure/left)))
    "S-<left>" (argless (if vemv/shortcuts/clojure/S-left (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-left vemv/shortcuts/clojure/S-left)))
    "<next>" (argless (if vemv/shortcuts/clojure/next (vemv/keyboard-funcall :vemv/shortcuts/clojure/next vemv/shortcuts/clojure/next)))
    "S-<next>" (argless (if vemv/shortcuts/clojure/S-next (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-next vemv/shortcuts/clojure/S-next)))
    "<prior>" (argless (if vemv/shortcuts/clojure/prior (vemv/keyboard-funcall :vemv/shortcuts/clojure/prior vemv/shortcuts/clojure/prior)))
    "S-<prior>" (argless (if vemv/shortcuts/clojure/S-prior (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-prior vemv/shortcuts/clojure/S-prior)))
    "<right>" (argless (if vemv/shortcuts/clojure/right (vemv/keyboard-funcall :vemv/shortcuts/clojure/right vemv/shortcuts/clojure/right)))
    "S-<right>" (argless (if vemv/shortcuts/clojure/S-right (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-right vemv/shortcuts/clojure/S-right)))
    "<up>" (argless (if vemv/shortcuts/clojure/up (vemv/keyboard-funcall :vemv/shortcuts/clojure/up vemv/shortcuts/clojure/up)))
    "S-<up>" (argless (if vemv/shortcuts/clojure/S-up (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-up vemv/shortcuts/clojure/S-up)))
    "RET" (argless (if vemv/shortcuts/clojure/RET (vemv/keyboard-funcall :vemv/shortcuts/clojure/RET vemv/shortcuts/clojure/RET)))
    "S-RET" (argless (if vemv/shortcuts/clojure/S-RET (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-RET vemv/shortcuts/clojure/S-RET)))
    "S-SPC" (argless (if vemv/shortcuts/clojure/S-SPC (vemv/keyboard-funcall :vemv/shortcuts/clojure/S-SPC vemv/shortcuts/clojure/S-SPC)))
    "C-a" (argless (if vemv/shortcuts/clojure/primary-a (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-a vemv/shortcuts/clojure/primary-a)))
    "C-M-a" (argless (if vemv/shortcuts/clojure/primary-secondary-a (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-a vemv/shortcuts/clojure/primary-secondary-a)))
    "C-S-a" (argless (if vemv/shortcuts/clojure/primary-S-a (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-a vemv/shortcuts/clojure/primary-S-a)))
    "C-b" (argless (if vemv/shortcuts/clojure/primary-b (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-b vemv/shortcuts/clojure/primary-b)))
    "C-M-b" (argless (if vemv/shortcuts/clojure/primary-secondary-b (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-b vemv/shortcuts/clojure/primary-secondary-b)))
    "C-S-b" (argless (if vemv/shortcuts/clojure/primary-S-b (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-b vemv/shortcuts/clojure/primary-S-b)))
    "C-c" (argless (if vemv/shortcuts/clojure/primary-c (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-c vemv/shortcuts/clojure/primary-c)))
    "C-M-c" (argless (if vemv/shortcuts/clojure/primary-secondary-c (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-c vemv/shortcuts/clojure/primary-secondary-c)))
    "C-S-c" (argless (if vemv/shortcuts/clojure/primary-S-c (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-c vemv/shortcuts/clojure/primary-S-c)))
    "C-d" (argless (if vemv/shortcuts/clojure/primary-d (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-d vemv/shortcuts/clojure/primary-d)))
    "C-M-d" (argless (if vemv/shortcuts/clojure/primary-secondary-d (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-d vemv/shortcuts/clojure/primary-secondary-d)))
    "C-S-d" (argless (if vemv/shortcuts/clojure/primary-S-d (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-d vemv/shortcuts/clojure/primary-S-d)))
    "C-e" (argless (if vemv/shortcuts/clojure/primary-e (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-e vemv/shortcuts/clojure/primary-e)))
    "C-M-e" (argless (if vemv/shortcuts/clojure/primary-secondary-e (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-e vemv/shortcuts/clojure/primary-secondary-e)))
    "C-S-e" (argless (if vemv/shortcuts/clojure/primary-S-e (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-e vemv/shortcuts/clojure/primary-S-e)))
    "C-f" (argless (if vemv/shortcuts/clojure/primary-f (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-f vemv/shortcuts/clojure/primary-f)))
    "C-M-f" (argless (if vemv/shortcuts/clojure/primary-secondary-f (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-f vemv/shortcuts/clojure/primary-secondary-f)))
    "C-S-f" (argless (if vemv/shortcuts/clojure/primary-S-f (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-f vemv/shortcuts/clojure/primary-S-f)))
    "C-h" (argless (if vemv/shortcuts/clojure/primary-h (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-h vemv/shortcuts/clojure/primary-h)))
    "C-M-h" (argless (if vemv/shortcuts/clojure/primary-secondary-h (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-h vemv/shortcuts/clojure/primary-secondary-h)))
    "C-S-h" (argless (if vemv/shortcuts/clojure/primary-S-h (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-h vemv/shortcuts/clojure/primary-S-h)))
    "C-j" (argless (if vemv/shortcuts/clojure/primary-j (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-j vemv/shortcuts/clojure/primary-j)))
    "C-M-j" (argless (if vemv/shortcuts/clojure/primary-secondary-j (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-j vemv/shortcuts/clojure/primary-secondary-j)))
    "C-S-j" (argless (if vemv/shortcuts/clojure/primary-S-j (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-j vemv/shortcuts/clojure/primary-S-j)))
    "C-k" (argless (if vemv/shortcuts/clojure/primary-k (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-k vemv/shortcuts/clojure/primary-k)))
    "C-M-k" (argless (if vemv/shortcuts/clojure/primary-secondary-k (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-k vemv/shortcuts/clojure/primary-secondary-k)))
    "C-S-k" (argless (if vemv/shortcuts/clojure/primary-S-k (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-k vemv/shortcuts/clojure/primary-S-k)))
    "C-l" (argless (if vemv/shortcuts/clojure/primary-l (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-l vemv/shortcuts/clojure/primary-l)))
    "C-M-l" (argless (if vemv/shortcuts/clojure/primary-secondary-l (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-l vemv/shortcuts/clojure/primary-secondary-l)))
    "C-S-l" (argless (if vemv/shortcuts/clojure/primary-S-l (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-l vemv/shortcuts/clojure/primary-S-l)))
    "C-n" (argless (if vemv/shortcuts/clojure/primary-n (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-n vemv/shortcuts/clojure/primary-n)))
    "C-M-n" (argless (if vemv/shortcuts/clojure/primary-secondary-n (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-n vemv/shortcuts/clojure/primary-secondary-n)))
    "C-S-n" (argless (if vemv/shortcuts/clojure/primary-S-n (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-n vemv/shortcuts/clojure/primary-S-n)))
    "C-o" (argless (if vemv/shortcuts/clojure/primary-o (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-o vemv/shortcuts/clojure/primary-o)))
    "C-M-o" (argless (if vemv/shortcuts/clojure/primary-secondary-o (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-o vemv/shortcuts/clojure/primary-secondary-o)))
    "C-S-o" (argless (if vemv/shortcuts/clojure/primary-S-o (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-o vemv/shortcuts/clojure/primary-S-o)))
    "C-p" (argless (if vemv/shortcuts/clojure/primary-p (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-p vemv/shortcuts/clojure/primary-p)))
    "C-M-p" (argless (if vemv/shortcuts/clojure/primary-secondary-p (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-p vemv/shortcuts/clojure/primary-secondary-p)))
    "C-S-p" (argless (if vemv/shortcuts/clojure/primary-S-p (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-p vemv/shortcuts/clojure/primary-S-p)))
    "C-q" (argless (if vemv/shortcuts/clojure/primary-q (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-q vemv/shortcuts/clojure/primary-q)))
    "C-M-q" (argless (if vemv/shortcuts/clojure/primary-secondary-q (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-q vemv/shortcuts/clojure/primary-secondary-q)))
    "C-S-q" (argless (if vemv/shortcuts/clojure/primary-S-q (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-q vemv/shortcuts/clojure/primary-S-q)))
    "C-r" (argless (if vemv/shortcuts/clojure/primary-r (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-r vemv/shortcuts/clojure/primary-r)))
    "C-M-r" (argless (if vemv/shortcuts/clojure/primary-secondary-r (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-r vemv/shortcuts/clojure/primary-secondary-r)))
    "C-S-r" (argless (if vemv/shortcuts/clojure/primary-S-r (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-r vemv/shortcuts/clojure/primary-S-r)))
    "C-s" (argless (if vemv/shortcuts/clojure/primary-s (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-s vemv/shortcuts/clojure/primary-s)))
    "C-M-s" (argless (if vemv/shortcuts/clojure/primary-secondary-s (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-s vemv/shortcuts/clojure/primary-secondary-s)))
    "C-S-s" (argless (if vemv/shortcuts/clojure/primary-S-s (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-s vemv/shortcuts/clojure/primary-S-s)))
    "C-t" (argless (if vemv/shortcuts/clojure/primary-t (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-t vemv/shortcuts/clojure/primary-t)))
    "C-M-t" (argless (if vemv/shortcuts/clojure/primary-secondary-t (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-t vemv/shortcuts/clojure/primary-secondary-t)))
    "C-S-t" (argless (if vemv/shortcuts/clojure/primary-S-t (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-t vemv/shortcuts/clojure/primary-S-t)))
    "C-u" (argless (if vemv/shortcuts/clojure/primary-u (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-u vemv/shortcuts/clojure/primary-u)))
    "C-M-u" (argless (if vemv/shortcuts/clojure/primary-secondary-u (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-u vemv/shortcuts/clojure/primary-secondary-u)))
    "C-S-u" (argless (if vemv/shortcuts/clojure/primary-S-u (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-u vemv/shortcuts/clojure/primary-S-u)))
    "C-v" (argless (if vemv/shortcuts/clojure/primary-v (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-v vemv/shortcuts/clojure/primary-v)))
    "C-M-v" (argless (if vemv/shortcuts/clojure/primary-secondary-v (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-v vemv/shortcuts/clojure/primary-secondary-v)))
    "C-S-v" (argless (if vemv/shortcuts/clojure/primary-S-v (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-v vemv/shortcuts/clojure/primary-S-v)))
    "C-w" (argless (if vemv/shortcuts/clojure/primary-w (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-w vemv/shortcuts/clojure/primary-w)))
    "C-M-w" (argless (if vemv/shortcuts/clojure/primary-secondary-w (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-w vemv/shortcuts/clojure/primary-secondary-w)))
    "C-S-w" (argless (if vemv/shortcuts/clojure/primary-S-w (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-w vemv/shortcuts/clojure/primary-S-w)))
    "C-x" (argless (if vemv/shortcuts/clojure/primary-x (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-x vemv/shortcuts/clojure/primary-x)))
    "C-M-x" (argless (if vemv/shortcuts/clojure/primary-secondary-x (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-x vemv/shortcuts/clojure/primary-secondary-x)))
    "C-S-x" (argless (if vemv/shortcuts/clojure/primary-S-x (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-x vemv/shortcuts/clojure/primary-S-x)))
    "C-y" (argless (if vemv/shortcuts/clojure/primary-y (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-y vemv/shortcuts/clojure/primary-y)))
    "C-M-y" (argless (if vemv/shortcuts/clojure/primary-secondary-y (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-y vemv/shortcuts/clojure/primary-secondary-y)))
    "C-S-y" (argless (if vemv/shortcuts/clojure/primary-S-y (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-y vemv/shortcuts/clojure/primary-S-y)))
    "C-z" (argless (if vemv/shortcuts/clojure/primary-z (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-z vemv/shortcuts/clojure/primary-z)))
    "C-M-z" (argless (if vemv/shortcuts/clojure/primary-secondary-z (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-z vemv/shortcuts/clojure/primary-secondary-z)))
    "C-S-z" (argless (if vemv/shortcuts/clojure/primary-S-z (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-z vemv/shortcuts/clojure/primary-S-z)))
    "C-1" (argless (if vemv/shortcuts/clojure/primary-1 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-1 vemv/shortcuts/clojure/primary-1)))
    "C-M-1" (argless (if vemv/shortcuts/clojure/primary-secondary-1 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-1 vemv/shortcuts/clojure/primary-secondary-1)))
    "C-S-1" (argless (if vemv/shortcuts/clojure/primary-S-1 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-1 vemv/shortcuts/clojure/primary-S-1)))
    "C-2" (argless (if vemv/shortcuts/clojure/primary-2 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-2 vemv/shortcuts/clojure/primary-2)))
    "C-M-2" (argless (if vemv/shortcuts/clojure/primary-secondary-2 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-2 vemv/shortcuts/clojure/primary-secondary-2)))
    "C-S-2" (argless (if vemv/shortcuts/clojure/primary-S-2 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-2 vemv/shortcuts/clojure/primary-S-2)))
    "C-3" (argless (if vemv/shortcuts/clojure/primary-3 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-3 vemv/shortcuts/clojure/primary-3)))
    "C-M-3" (argless (if vemv/shortcuts/clojure/primary-secondary-3 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-3 vemv/shortcuts/clojure/primary-secondary-3)))
    "C-S-3" (argless (if vemv/shortcuts/clojure/primary-S-3 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-3 vemv/shortcuts/clojure/primary-S-3)))
    "C-4" (argless (if vemv/shortcuts/clojure/primary-4 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-4 vemv/shortcuts/clojure/primary-4)))
    "C-M-4" (argless (if vemv/shortcuts/clojure/primary-secondary-4 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-4 vemv/shortcuts/clojure/primary-secondary-4)))
    "C-S-4" (argless (if vemv/shortcuts/clojure/primary-S-4 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-4 vemv/shortcuts/clojure/primary-S-4)))
    "C-5" (argless (if vemv/shortcuts/clojure/primary-5 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-5 vemv/shortcuts/clojure/primary-5)))
    "C-M-5" (argless (if vemv/shortcuts/clojure/primary-secondary-5 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-5 vemv/shortcuts/clojure/primary-secondary-5)))
    "C-S-5" (argless (if vemv/shortcuts/clojure/primary-S-5 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-5 vemv/shortcuts/clojure/primary-S-5)))
    "C-6" (argless (if vemv/shortcuts/clojure/primary-6 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-6 vemv/shortcuts/clojure/primary-6)))
    "C-M-6" (argless (if vemv/shortcuts/clojure/primary-secondary-6 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-6 vemv/shortcuts/clojure/primary-secondary-6)))
    "C-S-6" (argless (if vemv/shortcuts/clojure/primary-S-6 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-6 vemv/shortcuts/clojure/primary-S-6)))
    "C-7" (argless (if vemv/shortcuts/clojure/primary-7 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-7 vemv/shortcuts/clojure/primary-7)))
    "C-M-7" (argless (if vemv/shortcuts/clojure/primary-secondary-7 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-7 vemv/shortcuts/clojure/primary-secondary-7)))
    "C-S-7" (argless (if vemv/shortcuts/clojure/primary-S-7 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-7 vemv/shortcuts/clojure/primary-S-7)))
    "C-8" (argless (if vemv/shortcuts/clojure/primary-8 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-8 vemv/shortcuts/clojure/primary-8)))
    "C-M-8" (argless (if vemv/shortcuts/clojure/primary-secondary-8 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-8 vemv/shortcuts/clojure/primary-secondary-8)))
    "C-S-8" (argless (if vemv/shortcuts/clojure/primary-S-8 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-8 vemv/shortcuts/clojure/primary-S-8)))
    "C-9" (argless (if vemv/shortcuts/clojure/primary-9 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-9 vemv/shortcuts/clojure/primary-9)))
    "C-M-9" (argless (if vemv/shortcuts/clojure/primary-secondary-9 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-9 vemv/shortcuts/clojure/primary-secondary-9)))
    "C-S-9" (argless (if vemv/shortcuts/clojure/primary-S-9 (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-9 vemv/shortcuts/clojure/primary-S-9)))
    "C-!" (argless (if vemv/shortcuts/clojure/primary-bang (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-bang vemv/shortcuts/clojure/primary-bang)))
    "C-M-!" (argless (if vemv/shortcuts/clojure/primary-secondary-bang (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-bang vemv/shortcuts/clojure/primary-secondary-bang)))
    "C-S-!" (argless (if vemv/shortcuts/clojure/primary-S-bang (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-bang vemv/shortcuts/clojure/primary-S-bang)))
    "C-@" (argless (if vemv/shortcuts/clojure/primary-at (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-at vemv/shortcuts/clojure/primary-at)))
    "C-M-@" (argless (if vemv/shortcuts/clojure/primary-secondary-at (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-at vemv/shortcuts/clojure/primary-secondary-at)))
    "C-S-@" (argless (if vemv/shortcuts/clojure/primary-S-at (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-at vemv/shortcuts/clojure/primary-S-at)))
    "C-&" (argless (if vemv/shortcuts/clojure/primary-ampersand (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-ampersand vemv/shortcuts/clojure/primary-ampersand)))
    "C-M-&" (argless (if vemv/shortcuts/clojure/primary-secondary-ampersand (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-ampersand vemv/shortcuts/clojure/primary-secondary-ampersand)))
    "C-S-&" (argless (if vemv/shortcuts/clojure/primary-S-ampersand (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-ampersand vemv/shortcuts/clojure/primary-S-ampersand)))
    "C-#" (argless (if vemv/shortcuts/clojure/primary-hash (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-hash vemv/shortcuts/clojure/primary-hash)))
    "C-M-#" (argless (if vemv/shortcuts/clojure/primary-secondary-hash (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-hash vemv/shortcuts/clojure/primary-secondary-hash)))
    "C-S-#" (argless (if vemv/shortcuts/clojure/primary-S-hash (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-hash vemv/shortcuts/clojure/primary-S-hash)))
    "C-%" (argless (if vemv/shortcuts/clojure/primary-percent (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-percent vemv/shortcuts/clojure/primary-percent)))
    "C-M-%" (argless (if vemv/shortcuts/clojure/primary-secondary-percent (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-percent vemv/shortcuts/clojure/primary-secondary-percent)))
    "C-S-%" (argless (if vemv/shortcuts/clojure/primary-S-percent (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-percent vemv/shortcuts/clojure/primary-S-percent)))
    "C-^" (argless (if vemv/shortcuts/clojure/primary-caret (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-caret vemv/shortcuts/clojure/primary-caret)))
    "C-M-^" (argless (if vemv/shortcuts/clojure/primary-secondary-caret (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-caret vemv/shortcuts/clojure/primary-secondary-caret)))
    "C-S-^" (argless (if vemv/shortcuts/clojure/primary-S-caret (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-caret vemv/shortcuts/clojure/primary-S-caret)))
    "C-$" (argless (if vemv/shortcuts/clojure/primary-dollar (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-dollar vemv/shortcuts/clojure/primary-dollar)))
    "C-M-$" (argless (if vemv/shortcuts/clojure/primary-secondary-dollar (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-dollar vemv/shortcuts/clojure/primary-secondary-dollar)))
    "C-S-$" (argless (if vemv/shortcuts/clojure/primary-S-dollar (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-dollar vemv/shortcuts/clojure/primary-S-dollar)))
    "C-_" (argless (if vemv/shortcuts/clojure/primary-underscore (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-underscore vemv/shortcuts/clojure/primary-underscore)))
    "C-M-_" (argless (if vemv/shortcuts/clojure/primary-secondary-underscore (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-underscore vemv/shortcuts/clojure/primary-secondary-underscore)))
    "C-S-_" (argless (if vemv/shortcuts/clojure/primary-S-underscore (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-underscore vemv/shortcuts/clojure/primary-S-underscore)))
    "C--" (argless (if vemv/shortcuts/clojure/primary-dash (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-dash vemv/shortcuts/clojure/primary-dash)))
    "C-M--" (argless (if vemv/shortcuts/clojure/primary-secondary-dash (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-dash vemv/shortcuts/clojure/primary-secondary-dash)))
    "C-S--" (argless (if vemv/shortcuts/clojure/primary-S-dash (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-dash vemv/shortcuts/clojure/primary-S-dash)))
    "C-," (argless (if vemv/shortcuts/clojure/primary-comma (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-comma vemv/shortcuts/clojure/primary-comma)))
    "C-M-," (argless (if vemv/shortcuts/clojure/primary-secondary-comma (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-comma vemv/shortcuts/clojure/primary-secondary-comma)))
    "C-S-," (argless (if vemv/shortcuts/clojure/primary-S-comma (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-comma vemv/shortcuts/clojure/primary-S-comma)))
    "C-;" (argless (if vemv/shortcuts/clojure/primary-semicolon (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-semicolon vemv/shortcuts/clojure/primary-semicolon)))
    "C-M-;" (argless (if vemv/shortcuts/clojure/primary-secondary-semicolon (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-semicolon vemv/shortcuts/clojure/primary-secondary-semicolon)))
    "C-S-;" (argless (if vemv/shortcuts/clojure/primary-S-semicolon (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-semicolon vemv/shortcuts/clojure/primary-S-semicolon)))
    "C-:" (argless (if vemv/shortcuts/clojure/primary-colon (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-colon vemv/shortcuts/clojure/primary-colon)))
    "C-M-:" (argless (if vemv/shortcuts/clojure/primary-secondary-colon (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-colon vemv/shortcuts/clojure/primary-secondary-colon)))
    "C-S-:" (argless (if vemv/shortcuts/clojure/primary-S-colon (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-colon vemv/shortcuts/clojure/primary-S-colon)))
    "C-?" (argless (if vemv/shortcuts/clojure/primary-question-mark (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-question-mark vemv/shortcuts/clojure/primary-question-mark)))
    "C-M-?" (argless (if vemv/shortcuts/clojure/primary-secondary-question-mark (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-question-mark vemv/shortcuts/clojure/primary-secondary-question-mark)))
    "C-S-?" (argless (if vemv/shortcuts/clojure/primary-S-question-mark (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-question-mark vemv/shortcuts/clojure/primary-S-question-mark)))
    "C-." (argless (if vemv/shortcuts/clojure/primary-dot (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-dot vemv/shortcuts/clojure/primary-dot)))
    "C-M-." (argless (if vemv/shortcuts/clojure/primary-secondary-dot (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-dot vemv/shortcuts/clojure/primary-secondary-dot)))
    "C-S-." (argless (if vemv/shortcuts/clojure/primary-S-dot (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-dot vemv/shortcuts/clojure/primary-S-dot)))
    "C-'" (argless (if vemv/shortcuts/clojure/primary-single-quote (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-single-quote vemv/shortcuts/clojure/primary-single-quote)))
    "C-M-'" (argless (if vemv/shortcuts/clojure/primary-secondary-single-quote (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-single-quote vemv/shortcuts/clojure/primary-secondary-single-quote)))
    "C-S-'" (argless (if vemv/shortcuts/clojure/primary-S-single-quote (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-single-quote vemv/shortcuts/clojure/primary-S-single-quote)))
    "C-(" (argless (if vemv/shortcuts/clojure/primary-left-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-left-parens vemv/shortcuts/clojure/primary-left-parens)))
    "C-M-(" (argless (if vemv/shortcuts/clojure/primary-secondary-left-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-left-parens vemv/shortcuts/clojure/primary-secondary-left-parens)))
    "C-S-(" (argless (if vemv/shortcuts/clojure/primary-S-left-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-left-parens vemv/shortcuts/clojure/primary-S-left-parens)))
    "C-)" (argless (if vemv/shortcuts/clojure/primary-right-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-right-parens vemv/shortcuts/clojure/primary-right-parens)))
    "C-M-)" (argless (if vemv/shortcuts/clojure/primary-secondary-right-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-right-parens vemv/shortcuts/clojure/primary-secondary-right-parens)))
    "C-S-)" (argless (if vemv/shortcuts/clojure/primary-S-right-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-right-parens vemv/shortcuts/clojure/primary-S-right-parens)))
    "C-]" (argless (if vemv/shortcuts/clojure/primary-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-right-bracket vemv/shortcuts/clojure/primary-right-bracket)))
    "C-M-]" (argless (if vemv/shortcuts/clojure/primary-secondary-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-right-bracket vemv/shortcuts/clojure/primary-secondary-right-bracket)))
    "C-S-]" (argless (if vemv/shortcuts/clojure/primary-S-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-right-bracket vemv/shortcuts/clojure/primary-S-right-bracket)))
    "C-{" (argless (if vemv/shortcuts/clojure/primary-left-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-left-curly vemv/shortcuts/clojure/primary-left-curly)))
    "C-M-{" (argless (if vemv/shortcuts/clojure/primary-secondary-left-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-left-curly vemv/shortcuts/clojure/primary-secondary-left-curly)))
    "C-S-{" (argless (if vemv/shortcuts/clojure/primary-S-left-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-left-curly vemv/shortcuts/clojure/primary-S-left-curly)))
    "C-}" (argless (if vemv/shortcuts/clojure/primary-right-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-right-curly vemv/shortcuts/clojure/primary-right-curly)))
    "C-M-}" (argless (if vemv/shortcuts/clojure/primary-secondary-right-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-right-curly vemv/shortcuts/clojure/primary-secondary-right-curly)))
    "C-S-}" (argless (if vemv/shortcuts/clojure/primary-S-right-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-right-curly vemv/shortcuts/clojure/primary-S-right-curly)))
    "C-*" (argless (if vemv/shortcuts/clojure/primary-star (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-star vemv/shortcuts/clojure/primary-star)))
    "C-M-*" (argless (if vemv/shortcuts/clojure/primary-secondary-star (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-star vemv/shortcuts/clojure/primary-secondary-star)))
    "C-S-*" (argless (if vemv/shortcuts/clojure/primary-S-star (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-star vemv/shortcuts/clojure/primary-S-star)))
    "C-/" (argless (if vemv/shortcuts/clojure/primary-slash (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-slash vemv/shortcuts/clojure/primary-slash)))
    "C-M-/" (argless (if vemv/shortcuts/clojure/primary-secondary-slash (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-slash vemv/shortcuts/clojure/primary-secondary-slash)))
    "C-S-/" (argless (if vemv/shortcuts/clojure/primary-S-slash (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-slash vemv/shortcuts/clojure/primary-S-slash)))
    "C-`" (argless (if vemv/shortcuts/clojure/primary-backtick (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-backtick vemv/shortcuts/clojure/primary-backtick)))
    "C-M-`" (argless (if vemv/shortcuts/clojure/primary-secondary-backtick (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-backtick vemv/shortcuts/clojure/primary-secondary-backtick)))
    "C-S-`" (argless (if vemv/shortcuts/clojure/primary-S-backtick (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-backtick vemv/shortcuts/clojure/primary-S-backtick)))
    "C-+" (argless (if vemv/shortcuts/clojure/primary-plus (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-plus vemv/shortcuts/clojure/primary-plus)))
    "C-M-+" (argless (if vemv/shortcuts/clojure/primary-secondary-plus (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-plus vemv/shortcuts/clojure/primary-secondary-plus)))
    "C-S-+" (argless (if vemv/shortcuts/clojure/primary-S-plus (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-plus vemv/shortcuts/clojure/primary-S-plus)))
    "C-<backspace>" (argless (if vemv/shortcuts/clojure/primary-backspace (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-backspace vemv/shortcuts/clojure/primary-backspace)))
    "C-M-<backspace>" (argless (if vemv/shortcuts/clojure/primary-secondary-backspace (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-backspace vemv/shortcuts/clojure/primary-secondary-backspace)))
    "C-S-<backspace>" (argless (if vemv/shortcuts/clojure/primary-S-backspace (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-backspace vemv/shortcuts/clojure/primary-S-backspace)))
    "C-<down>" (argless (if vemv/shortcuts/clojure/primary-down (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-down vemv/shortcuts/clojure/primary-down)))
    "C-M-<down>" (argless (if vemv/shortcuts/clojure/primary-secondary-down (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-down vemv/shortcuts/clojure/primary-secondary-down)))
    "C-S-<down>" (argless (if vemv/shortcuts/clojure/primary-S-down (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-down vemv/shortcuts/clojure/primary-S-down)))
    "C-<end>" (argless (if vemv/shortcuts/clojure/primary-end (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-end vemv/shortcuts/clojure/primary-end)))
    "C-M-<end>" (argless (if vemv/shortcuts/clojure/primary-secondary-end (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-end vemv/shortcuts/clojure/primary-secondary-end)))
    "C-S-<end>" (argless (if vemv/shortcuts/clojure/primary-S-end (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-end vemv/shortcuts/clojure/primary-S-end)))
    "C-<home>" (argless (if vemv/shortcuts/clojure/primary-home (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-home vemv/shortcuts/clojure/primary-home)))
    "C-M-<home>" (argless (if vemv/shortcuts/clojure/primary-secondary-home (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-home vemv/shortcuts/clojure/primary-secondary-home)))
    "C-S-<home>" (argless (if vemv/shortcuts/clojure/primary-S-home (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-home vemv/shortcuts/clojure/primary-S-home)))
    "C-<left>" (argless (if vemv/shortcuts/clojure/primary-left (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-left vemv/shortcuts/clojure/primary-left)))
    "C-M-<left>" (argless (if vemv/shortcuts/clojure/primary-secondary-left (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-left vemv/shortcuts/clojure/primary-secondary-left)))
    "C-S-<left>" (argless (if vemv/shortcuts/clojure/primary-S-left (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-left vemv/shortcuts/clojure/primary-S-left)))
    "C-<next>" (argless (if vemv/shortcuts/clojure/primary-next (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-next vemv/shortcuts/clojure/primary-next)))
    "C-M-<next>" (argless (if vemv/shortcuts/clojure/primary-secondary-next (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-next vemv/shortcuts/clojure/primary-secondary-next)))
    "C-S-<next>" (argless (if vemv/shortcuts/clojure/primary-S-next (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-next vemv/shortcuts/clojure/primary-S-next)))
    "C-<prior>" (argless (if vemv/shortcuts/clojure/primary-prior (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-prior vemv/shortcuts/clojure/primary-prior)))
    "C-M-<prior>" (argless (if vemv/shortcuts/clojure/primary-secondary-prior (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-prior vemv/shortcuts/clojure/primary-secondary-prior)))
    "C-S-<prior>" (argless (if vemv/shortcuts/clojure/primary-S-prior (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-prior vemv/shortcuts/clojure/primary-S-prior)))
    "C-<right>" (argless (if vemv/shortcuts/clojure/primary-right (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-right vemv/shortcuts/clojure/primary-right)))
    "C-M-<right>" (argless (if vemv/shortcuts/clojure/primary-secondary-right (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-right vemv/shortcuts/clojure/primary-secondary-right)))
    "C-S-<right>" (argless (if vemv/shortcuts/clojure/primary-S-right (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-right vemv/shortcuts/clojure/primary-S-right)))
    "C-<up>" (argless (if vemv/shortcuts/clojure/primary-up (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-up vemv/shortcuts/clojure/primary-up)))
    "C-M-<up>" (argless (if vemv/shortcuts/clojure/primary-secondary-up (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-up vemv/shortcuts/clojure/primary-secondary-up)))
    "C-S-<up>" (argless (if vemv/shortcuts/clojure/primary-S-up (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-up vemv/shortcuts/clojure/primary-S-up)))
    "C-=" (argless (if vemv/shortcuts/clojure/primary-equal (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-equal vemv/shortcuts/clojure/primary-equal)))
    "C-M-=" (argless (if vemv/shortcuts/clojure/primary-secondary-equal (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-equal vemv/shortcuts/clojure/primary-secondary-equal)))
    "C-S-=" (argless (if vemv/shortcuts/clojure/primary-S-equal (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-equal vemv/shortcuts/clojure/primary-S-equal)))
    "C-|" (argless (if vemv/shortcuts/clojure/primary-bar (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-bar vemv/shortcuts/clojure/primary-bar)))
    "C-M-|" (argless (if vemv/shortcuts/clojure/primary-secondary-bar (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-bar vemv/shortcuts/clojure/primary-secondary-bar)))
    "C-S-|" (argless (if vemv/shortcuts/clojure/primary-S-bar (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-bar vemv/shortcuts/clojure/primary-S-bar)))
    "C-RET" (argless (if vemv/shortcuts/clojure/primary-RET (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-RET vemv/shortcuts/clojure/primary-RET)))
    "C-M-RET" (argless (if vemv/shortcuts/clojure/primary-secondary-RET (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-RET vemv/shortcuts/clojure/primary-secondary-RET)))
    "C-S-RET" (argless (if vemv/shortcuts/clojure/primary-S-RET (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-RET vemv/shortcuts/clojure/primary-S-RET)))
    "C-SPC" (argless (if vemv/shortcuts/clojure/primary-SPC (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-SPC vemv/shortcuts/clojure/primary-SPC)))
    "C-M-SPC" (argless (if vemv/shortcuts/clojure/primary-secondary-SPC (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-secondary-SPC vemv/shortcuts/clojure/primary-secondary-SPC)))
    "C-S-SPC" (argless (if vemv/shortcuts/clojure/primary-S-SPC (vemv/keyboard-funcall :vemv/shortcuts/clojure/primary-S-SPC vemv/shortcuts/clojure/primary-S-SPC)))
    "M-a" (argless (if vemv/shortcuts/clojure/secondary-a (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-a vemv/shortcuts/clojure/secondary-a)))
    "M-S-a" (argless (if vemv/shortcuts/clojure/secondary-S-a (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-a vemv/shortcuts/clojure/secondary-S-a)))
    "M-b" (argless (if vemv/shortcuts/clojure/secondary-b (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-b vemv/shortcuts/clojure/secondary-b)))
    "M-S-b" (argless (if vemv/shortcuts/clojure/secondary-S-b (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-b vemv/shortcuts/clojure/secondary-S-b)))
    "M-c" (argless (if vemv/shortcuts/clojure/secondary-c (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-c vemv/shortcuts/clojure/secondary-c)))
    "M-S-c" (argless (if vemv/shortcuts/clojure/secondary-S-c (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-c vemv/shortcuts/clojure/secondary-S-c)))
    "M-d" (argless (if vemv/shortcuts/clojure/secondary-d (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-d vemv/shortcuts/clojure/secondary-d)))
    "M-S-d" (argless (if vemv/shortcuts/clojure/secondary-S-d (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-d vemv/shortcuts/clojure/secondary-S-d)))
    "M-e" (argless (if vemv/shortcuts/clojure/secondary-e (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-e vemv/shortcuts/clojure/secondary-e)))
    "M-S-e" (argless (if vemv/shortcuts/clojure/secondary-S-e (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-e vemv/shortcuts/clojure/secondary-S-e)))
    "M-f" (argless (if vemv/shortcuts/clojure/secondary-f (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-f vemv/shortcuts/clojure/secondary-f)))
    "M-S-f" (argless (if vemv/shortcuts/clojure/secondary-S-f (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-f vemv/shortcuts/clojure/secondary-S-f)))
    "M-g" (argless (if vemv/shortcuts/clojure/secondary-g (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-g vemv/shortcuts/clojure/secondary-g)))
    "M-S-g" (argless (if vemv/shortcuts/clojure/secondary-S-g (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-g vemv/shortcuts/clojure/secondary-S-g)))
    "M-h" (argless (if vemv/shortcuts/clojure/secondary-h (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-h vemv/shortcuts/clojure/secondary-h)))
    "M-S-h" (argless (if vemv/shortcuts/clojure/secondary-S-h (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-h vemv/shortcuts/clojure/secondary-S-h)))
    "M-i" (argless (if vemv/shortcuts/clojure/secondary-i (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-i vemv/shortcuts/clojure/secondary-i)))
    "M-S-i" (argless (if vemv/shortcuts/clojure/secondary-S-i (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-i vemv/shortcuts/clojure/secondary-S-i)))
    "M-j" (argless (if vemv/shortcuts/clojure/secondary-j (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-j vemv/shortcuts/clojure/secondary-j)))
    "M-S-j" (argless (if vemv/shortcuts/clojure/secondary-S-j (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-j vemv/shortcuts/clojure/secondary-S-j)))
    "M-k" (argless (if vemv/shortcuts/clojure/secondary-k (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-k vemv/shortcuts/clojure/secondary-k)))
    "M-S-k" (argless (if vemv/shortcuts/clojure/secondary-S-k (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-k vemv/shortcuts/clojure/secondary-S-k)))
    "M-l" (argless (if vemv/shortcuts/clojure/secondary-l (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-l vemv/shortcuts/clojure/secondary-l)))
    "M-S-l" (argless (if vemv/shortcuts/clojure/secondary-S-l (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-l vemv/shortcuts/clojure/secondary-S-l)))
    "M-m" (argless (if vemv/shortcuts/clojure/secondary-m (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-m vemv/shortcuts/clojure/secondary-m)))
    "M-S-m" (argless (if vemv/shortcuts/clojure/secondary-S-m (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-m vemv/shortcuts/clojure/secondary-S-m)))
    "M-n" (argless (if vemv/shortcuts/clojure/secondary-n (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-n vemv/shortcuts/clojure/secondary-n)))
    "M-S-n" (argless (if vemv/shortcuts/clojure/secondary-S-n (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-n vemv/shortcuts/clojure/secondary-S-n)))
    "M-o" (argless (if vemv/shortcuts/clojure/secondary-o (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-o vemv/shortcuts/clojure/secondary-o)))
    "M-S-o" (argless (if vemv/shortcuts/clojure/secondary-S-o (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-o vemv/shortcuts/clojure/secondary-S-o)))
    "M-p" (argless (if vemv/shortcuts/clojure/secondary-p (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-p vemv/shortcuts/clojure/secondary-p)))
    "M-S-p" (argless (if vemv/shortcuts/clojure/secondary-S-p (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-p vemv/shortcuts/clojure/secondary-S-p)))
    "M-q" (argless (if vemv/shortcuts/clojure/secondary-q (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-q vemv/shortcuts/clojure/secondary-q)))
    "M-S-q" (argless (if vemv/shortcuts/clojure/secondary-S-q (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-q vemv/shortcuts/clojure/secondary-S-q)))
    "M-r" (argless (if vemv/shortcuts/clojure/secondary-r (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-r vemv/shortcuts/clojure/secondary-r)))
    "M-S-r" (argless (if vemv/shortcuts/clojure/secondary-S-r (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-r vemv/shortcuts/clojure/secondary-S-r)))
    "M-s" (argless (if vemv/shortcuts/clojure/secondary-s (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-s vemv/shortcuts/clojure/secondary-s)))
    "M-S-s" (argless (if vemv/shortcuts/clojure/secondary-S-s (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-s vemv/shortcuts/clojure/secondary-S-s)))
    "M-t" (argless (if vemv/shortcuts/clojure/secondary-t (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-t vemv/shortcuts/clojure/secondary-t)))
    "M-S-t" (argless (if vemv/shortcuts/clojure/secondary-S-t (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-t vemv/shortcuts/clojure/secondary-S-t)))
    "M-u" (argless (if vemv/shortcuts/clojure/secondary-u (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-u vemv/shortcuts/clojure/secondary-u)))
    "M-S-u" (argless (if vemv/shortcuts/clojure/secondary-S-u (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-u vemv/shortcuts/clojure/secondary-S-u)))
    "M-v" (argless (if vemv/shortcuts/clojure/secondary-v (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-v vemv/shortcuts/clojure/secondary-v)))
    "M-S-v" (argless (if vemv/shortcuts/clojure/secondary-S-v (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-v vemv/shortcuts/clojure/secondary-S-v)))
    "M-w" (argless (if vemv/shortcuts/clojure/secondary-w (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-w vemv/shortcuts/clojure/secondary-w)))
    "M-S-w" (argless (if vemv/shortcuts/clojure/secondary-S-w (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-w vemv/shortcuts/clojure/secondary-S-w)))
    "M-x" (argless (if vemv/shortcuts/clojure/secondary-x (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-x vemv/shortcuts/clojure/secondary-x)))
    "M-S-x" (argless (if vemv/shortcuts/clojure/secondary-S-x (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-x vemv/shortcuts/clojure/secondary-S-x)))
    "M-y" (argless (if vemv/shortcuts/clojure/secondary-y (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-y vemv/shortcuts/clojure/secondary-y)))
    "M-S-y" (argless (if vemv/shortcuts/clojure/secondary-S-y (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-y vemv/shortcuts/clojure/secondary-S-y)))
    "M-z" (argless (if vemv/shortcuts/clojure/secondary-z (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-z vemv/shortcuts/clojure/secondary-z)))
    "M-S-z" (argless (if vemv/shortcuts/clojure/secondary-S-z (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-z vemv/shortcuts/clojure/secondary-S-z)))
    "M-0" (argless (if vemv/shortcuts/clojure/secondary-0 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-0 vemv/shortcuts/clojure/secondary-0)))
    "M-S-0" (argless (if vemv/shortcuts/clojure/secondary-S-0 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-0 vemv/shortcuts/clojure/secondary-S-0)))
    "M-1" (argless (if vemv/shortcuts/clojure/secondary-1 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-1 vemv/shortcuts/clojure/secondary-1)))
    "M-S-1" (argless (if vemv/shortcuts/clojure/secondary-S-1 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-1 vemv/shortcuts/clojure/secondary-S-1)))
    "M-2" (argless (if vemv/shortcuts/clojure/secondary-2 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-2 vemv/shortcuts/clojure/secondary-2)))
    "M-S-2" (argless (if vemv/shortcuts/clojure/secondary-S-2 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-2 vemv/shortcuts/clojure/secondary-S-2)))
    "M-3" (argless (if vemv/shortcuts/clojure/secondary-3 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-3 vemv/shortcuts/clojure/secondary-3)))
    "M-S-3" (argless (if vemv/shortcuts/clojure/secondary-S-3 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-3 vemv/shortcuts/clojure/secondary-S-3)))
    "M-4" (argless (if vemv/shortcuts/clojure/secondary-4 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-4 vemv/shortcuts/clojure/secondary-4)))
    "M-S-4" (argless (if vemv/shortcuts/clojure/secondary-S-4 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-4 vemv/shortcuts/clojure/secondary-S-4)))
    "M-5" (argless (if vemv/shortcuts/clojure/secondary-5 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-5 vemv/shortcuts/clojure/secondary-5)))
    "M-S-5" (argless (if vemv/shortcuts/clojure/secondary-S-5 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-5 vemv/shortcuts/clojure/secondary-S-5)))
    "M-6" (argless (if vemv/shortcuts/clojure/secondary-6 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-6 vemv/shortcuts/clojure/secondary-6)))
    "M-S-6" (argless (if vemv/shortcuts/clojure/secondary-S-6 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-6 vemv/shortcuts/clojure/secondary-S-6)))
    "M-7" (argless (if vemv/shortcuts/clojure/secondary-7 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-7 vemv/shortcuts/clojure/secondary-7)))
    "M-S-7" (argless (if vemv/shortcuts/clojure/secondary-S-7 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-7 vemv/shortcuts/clojure/secondary-S-7)))
    "M-8" (argless (if vemv/shortcuts/clojure/secondary-8 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-8 vemv/shortcuts/clojure/secondary-8)))
    "M-S-8" (argless (if vemv/shortcuts/clojure/secondary-S-8 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-8 vemv/shortcuts/clojure/secondary-S-8)))
    "M-9" (argless (if vemv/shortcuts/clojure/secondary-9 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-9 vemv/shortcuts/clojure/secondary-9)))
    "M-S-9" (argless (if vemv/shortcuts/clojure/secondary-S-9 (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-9 vemv/shortcuts/clojure/secondary-S-9)))
    "M-!" (argless (if vemv/shortcuts/clojure/secondary-bang (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-bang vemv/shortcuts/clojure/secondary-bang)))
    "M-S-!" (argless (if vemv/shortcuts/clojure/secondary-S-bang (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-bang vemv/shortcuts/clojure/secondary-S-bang)))
    "M-@" (argless (if vemv/shortcuts/clojure/secondary-at (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-at vemv/shortcuts/clojure/secondary-at)))
    "M-S-@" (argless (if vemv/shortcuts/clojure/secondary-S-at (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-at vemv/shortcuts/clojure/secondary-S-at)))
    "M-&" (argless (if vemv/shortcuts/clojure/secondary-ampersand (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-ampersand vemv/shortcuts/clojure/secondary-ampersand)))
    "M-S-&" (argless (if vemv/shortcuts/clojure/secondary-S-ampersand (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-ampersand vemv/shortcuts/clojure/secondary-S-ampersand)))
    "M-#" (argless (if vemv/shortcuts/clojure/secondary-hash (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-hash vemv/shortcuts/clojure/secondary-hash)))
    "M-S-#" (argless (if vemv/shortcuts/clojure/secondary-S-hash (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-hash vemv/shortcuts/clojure/secondary-S-hash)))
    "M-%" (argless (if vemv/shortcuts/clojure/secondary-percent (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-percent vemv/shortcuts/clojure/secondary-percent)))
    "M-S-%" (argless (if vemv/shortcuts/clojure/secondary-S-percent (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-percent vemv/shortcuts/clojure/secondary-S-percent)))
    "M-^" (argless (if vemv/shortcuts/clojure/secondary-caret (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-caret vemv/shortcuts/clojure/secondary-caret)))
    "M-S-^" (argless (if vemv/shortcuts/clojure/secondary-S-caret (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-caret vemv/shortcuts/clojure/secondary-S-caret)))
    "M-~" (argless (if vemv/shortcuts/clojure/secondary-tilde (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-tilde vemv/shortcuts/clojure/secondary-tilde)))
    "M-S-~" (argless (if vemv/shortcuts/clojure/secondary-S-tilde (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-tilde vemv/shortcuts/clojure/secondary-S-tilde)))
    "M-$" (argless (if vemv/shortcuts/clojure/secondary-dollar (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-dollar vemv/shortcuts/clojure/secondary-dollar)))
    "M-S-$" (argless (if vemv/shortcuts/clojure/secondary-S-dollar (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-dollar vemv/shortcuts/clojure/secondary-S-dollar)))
    "M-_" (argless (if vemv/shortcuts/clojure/secondary-underscore (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-underscore vemv/shortcuts/clojure/secondary-underscore)))
    "M-S-_" (argless (if vemv/shortcuts/clojure/secondary-S-underscore (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-underscore vemv/shortcuts/clojure/secondary-S-underscore)))
    "M--" (argless (if vemv/shortcuts/clojure/secondary-dash (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-dash vemv/shortcuts/clojure/secondary-dash)))
    "M-S--" (argless (if vemv/shortcuts/clojure/secondary-S-dash (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-dash vemv/shortcuts/clojure/secondary-S-dash)))
    "M-," (argless (if vemv/shortcuts/clojure/secondary-comma (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-comma vemv/shortcuts/clojure/secondary-comma)))
    "M-S-," (argless (if vemv/shortcuts/clojure/secondary-S-comma (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-comma vemv/shortcuts/clojure/secondary-S-comma)))
    "M-;" (argless (if vemv/shortcuts/clojure/secondary-semicolon (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-semicolon vemv/shortcuts/clojure/secondary-semicolon)))
    "M-S-;" (argless (if vemv/shortcuts/clojure/secondary-S-semicolon (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-semicolon vemv/shortcuts/clojure/secondary-S-semicolon)))
    "M-:" (argless (if vemv/shortcuts/clojure/secondary-colon (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-colon vemv/shortcuts/clojure/secondary-colon)))
    "M-S-:" (argless (if vemv/shortcuts/clojure/secondary-S-colon (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-colon vemv/shortcuts/clojure/secondary-S-colon)))
    "M-?" (argless (if vemv/shortcuts/clojure/secondary-question-mark (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-question-mark vemv/shortcuts/clojure/secondary-question-mark)))
    "M-S-?" (argless (if vemv/shortcuts/clojure/secondary-S-question-mark (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-question-mark vemv/shortcuts/clojure/secondary-S-question-mark)))
    "M-." (argless (if vemv/shortcuts/clojure/secondary-dot (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-dot vemv/shortcuts/clojure/secondary-dot)))
    "M-S-." (argless (if vemv/shortcuts/clojure/secondary-S-dot (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-dot vemv/shortcuts/clojure/secondary-S-dot)))
    "M-'" (argless (if vemv/shortcuts/clojure/secondary-single-quote (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-single-quote vemv/shortcuts/clojure/secondary-single-quote)))
    "M-S-'" (argless (if vemv/shortcuts/clojure/secondary-S-single-quote (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-single-quote vemv/shortcuts/clojure/secondary-S-single-quote)))
    "M-(" (argless (if vemv/shortcuts/clojure/secondary-left-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-left-parens vemv/shortcuts/clojure/secondary-left-parens)))
    "M-S-(" (argless (if vemv/shortcuts/clojure/secondary-S-left-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-left-parens vemv/shortcuts/clojure/secondary-S-left-parens)))
    "M-)" (argless (if vemv/shortcuts/clojure/secondary-right-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-right-parens vemv/shortcuts/clojure/secondary-right-parens)))
    "M-S-)" (argless (if vemv/shortcuts/clojure/secondary-S-right-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-right-parens vemv/shortcuts/clojure/secondary-S-right-parens)))
    "M-[" (argless (if vemv/shortcuts/clojure/secondary-left-bracket (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-left-bracket vemv/shortcuts/clojure/secondary-left-bracket)))
    "M-S-[" (argless (if vemv/shortcuts/clojure/secondary-S-left-bracket (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-left-bracket vemv/shortcuts/clojure/secondary-S-left-bracket)))
    "M-]" (argless (if vemv/shortcuts/clojure/secondary-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-right-bracket vemv/shortcuts/clojure/secondary-right-bracket)))
    "M-S-]" (argless (if vemv/shortcuts/clojure/secondary-S-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-right-bracket vemv/shortcuts/clojure/secondary-S-right-bracket)))
    "M-{" (argless (if vemv/shortcuts/clojure/secondary-left-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-left-curly vemv/shortcuts/clojure/secondary-left-curly)))
    "M-S-{" (argless (if vemv/shortcuts/clojure/secondary-S-left-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-left-curly vemv/shortcuts/clojure/secondary-S-left-curly)))
    "M-}" (argless (if vemv/shortcuts/clojure/secondary-right-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-right-curly vemv/shortcuts/clojure/secondary-right-curly)))
    "M-S-}" (argless (if vemv/shortcuts/clojure/secondary-S-right-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-right-curly vemv/shortcuts/clojure/secondary-S-right-curly)))
    "M-*" (argless (if vemv/shortcuts/clojure/secondary-star (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-star vemv/shortcuts/clojure/secondary-star)))
    "M-S-*" (argless (if vemv/shortcuts/clojure/secondary-S-star (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-star vemv/shortcuts/clojure/secondary-S-star)))
    "M-/" (argless (if vemv/shortcuts/clojure/secondary-slash (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-slash vemv/shortcuts/clojure/secondary-slash)))
    "M-S-/" (argless (if vemv/shortcuts/clojure/secondary-S-slash (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-slash vemv/shortcuts/clojure/secondary-S-slash)))
    "M-`" (argless (if vemv/shortcuts/clojure/secondary-backtick (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-backtick vemv/shortcuts/clojure/secondary-backtick)))
    "M-S-`" (argless (if vemv/shortcuts/clojure/secondary-S-backtick (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-backtick vemv/shortcuts/clojure/secondary-S-backtick)))
    "M-+" (argless (if vemv/shortcuts/clojure/secondary-plus (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-plus vemv/shortcuts/clojure/secondary-plus)))
    "M-S-+" (argless (if vemv/shortcuts/clojure/secondary-S-plus (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-plus vemv/shortcuts/clojure/secondary-S-plus)))
    "M-<backspace>" (argless (if vemv/shortcuts/clojure/secondary-backspace (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-backspace vemv/shortcuts/clojure/secondary-backspace)))
    "M-S-<backspace>" (argless (if vemv/shortcuts/clojure/secondary-S-backspace (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-backspace vemv/shortcuts/clojure/secondary-S-backspace)))
    "M-<down>" (argless (if vemv/shortcuts/clojure/secondary-down (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-down vemv/shortcuts/clojure/secondary-down)))
    "M-S-<down>" (argless (if vemv/shortcuts/clojure/secondary-S-down (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-down vemv/shortcuts/clojure/secondary-S-down)))
    "M-<end>" (argless (if vemv/shortcuts/clojure/secondary-end (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-end vemv/shortcuts/clojure/secondary-end)))
    "M-S-<end>" (argless (if vemv/shortcuts/clojure/secondary-S-end (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-end vemv/shortcuts/clojure/secondary-S-end)))
    "M-<home>" (argless (if vemv/shortcuts/clojure/secondary-home (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-home vemv/shortcuts/clojure/secondary-home)))
    "M-S-<home>" (argless (if vemv/shortcuts/clojure/secondary-S-home (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-home vemv/shortcuts/clojure/secondary-S-home)))
    "M-<left>" (argless (if vemv/shortcuts/clojure/secondary-left (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-left vemv/shortcuts/clojure/secondary-left)))
    "M-S-<left>" (argless (if vemv/shortcuts/clojure/secondary-S-left (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-left vemv/shortcuts/clojure/secondary-S-left)))
    "M-<next>" (argless (if vemv/shortcuts/clojure/secondary-next (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-next vemv/shortcuts/clojure/secondary-next)))
    "M-S-<next>" (argless (if vemv/shortcuts/clojure/secondary-S-next (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-next vemv/shortcuts/clojure/secondary-S-next)))
    "M-<prior>" (argless (if vemv/shortcuts/clojure/secondary-prior (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-prior vemv/shortcuts/clojure/secondary-prior)))
    "M-S-<prior>" (argless (if vemv/shortcuts/clojure/secondary-S-prior (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-prior vemv/shortcuts/clojure/secondary-S-prior)))
    "M-<right>" (argless (if vemv/shortcuts/clojure/secondary-right (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-right vemv/shortcuts/clojure/secondary-right)))
    "M-S-<right>" (argless (if vemv/shortcuts/clojure/secondary-S-right (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-right vemv/shortcuts/clojure/secondary-S-right)))
    "M-<up>" (argless (if vemv/shortcuts/clojure/secondary-up (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-up vemv/shortcuts/clojure/secondary-up)))
    "M-S-<up>" (argless (if vemv/shortcuts/clojure/secondary-S-up (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-up vemv/shortcuts/clojure/secondary-S-up)))
    "M-=" (argless (if vemv/shortcuts/clojure/secondary-equal (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-equal vemv/shortcuts/clojure/secondary-equal)))
    "M-S-=" (argless (if vemv/shortcuts/clojure/secondary-S-equal (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-equal vemv/shortcuts/clojure/secondary-S-equal)))
    "M-|" (argless (if vemv/shortcuts/clojure/secondary-bar (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-bar vemv/shortcuts/clojure/secondary-bar)))
    "M-S-|" (argless (if vemv/shortcuts/clojure/secondary-S-bar (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-bar vemv/shortcuts/clojure/secondary-S-bar)))
    "M-RET" (argless (if vemv/shortcuts/clojure/secondary-RET (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-RET vemv/shortcuts/clojure/secondary-RET)))
    "M-S-RET" (argless (if vemv/shortcuts/clojure/secondary-S-RET (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-RET vemv/shortcuts/clojure/secondary-S-RET)))
    "M-SPC" (argless (if vemv/shortcuts/clojure/secondary-SPC (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-SPC vemv/shortcuts/clojure/secondary-SPC)))
    "M-S-SPC" (argless (if vemv/shortcuts/clojure/secondary-S-SPC (vemv/keyboard-funcall :vemv/shortcuts/clojure/secondary-S-SPC vemv/shortcuts/clojure/secondary-S-SPC)))
    "s-a" (argless (if vemv/shortcuts/clojure/tertiary-a (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-a vemv/shortcuts/clojure/tertiary-a)))
    "s-S-a" (argless (if vemv/shortcuts/clojure/tertiary-S-a (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-a vemv/shortcuts/clojure/tertiary-S-a)))
    "s-b" (argless (if vemv/shortcuts/clojure/tertiary-b (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-b vemv/shortcuts/clojure/tertiary-b)))
    "s-S-b" (argless (if vemv/shortcuts/clojure/tertiary-S-b (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-b vemv/shortcuts/clojure/tertiary-S-b)))
    "s-c" (argless (if vemv/shortcuts/clojure/tertiary-c (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-c vemv/shortcuts/clojure/tertiary-c)))
    "s-S-c" (argless (if vemv/shortcuts/clojure/tertiary-S-c (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-c vemv/shortcuts/clojure/tertiary-S-c)))
    "s-d" (argless (if vemv/shortcuts/clojure/tertiary-d (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-d vemv/shortcuts/clojure/tertiary-d)))
    "s-S-d" (argless (if vemv/shortcuts/clojure/tertiary-S-d (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-d vemv/shortcuts/clojure/tertiary-S-d)))
    "s-e" (argless (if vemv/shortcuts/clojure/tertiary-e (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-e vemv/shortcuts/clojure/tertiary-e)))
    "s-S-e" (argless (if vemv/shortcuts/clojure/tertiary-S-e (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-e vemv/shortcuts/clojure/tertiary-S-e)))
    "s-f" (argless (if vemv/shortcuts/clojure/tertiary-f (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-f vemv/shortcuts/clojure/tertiary-f)))
    "s-S-f" (argless (if vemv/shortcuts/clojure/tertiary-S-f (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-f vemv/shortcuts/clojure/tertiary-S-f)))
    "s-g" (argless (if vemv/shortcuts/clojure/tertiary-g (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-g vemv/shortcuts/clojure/tertiary-g)))
    "s-S-g" (argless (if vemv/shortcuts/clojure/tertiary-S-g (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-g vemv/shortcuts/clojure/tertiary-S-g)))
    "s-h" (argless (if vemv/shortcuts/clojure/tertiary-h (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-h vemv/shortcuts/clojure/tertiary-h)))
    "s-S-h" (argless (if vemv/shortcuts/clojure/tertiary-S-h (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-h vemv/shortcuts/clojure/tertiary-S-h)))
    "s-i" (argless (if vemv/shortcuts/clojure/tertiary-i (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-i vemv/shortcuts/clojure/tertiary-i)))
    "s-S-i" (argless (if vemv/shortcuts/clojure/tertiary-S-i (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-i vemv/shortcuts/clojure/tertiary-S-i)))
    "s-j" (argless (if vemv/shortcuts/clojure/tertiary-j (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-j vemv/shortcuts/clojure/tertiary-j)))
    "s-S-j" (argless (if vemv/shortcuts/clojure/tertiary-S-j (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-j vemv/shortcuts/clojure/tertiary-S-j)))
    "s-k" (argless (if vemv/shortcuts/clojure/tertiary-k (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-k vemv/shortcuts/clojure/tertiary-k)))
    "s-S-k" (argless (if vemv/shortcuts/clojure/tertiary-S-k (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-k vemv/shortcuts/clojure/tertiary-S-k)))
    "s-l" (argless (if vemv/shortcuts/clojure/tertiary-l (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-l vemv/shortcuts/clojure/tertiary-l)))
    "s-S-l" (argless (if vemv/shortcuts/clojure/tertiary-S-l (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-l vemv/shortcuts/clojure/tertiary-S-l)))
    "s-m" (argless (if vemv/shortcuts/clojure/tertiary-m (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-m vemv/shortcuts/clojure/tertiary-m)))
    "s-S-m" (argless (if vemv/shortcuts/clojure/tertiary-S-m (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-m vemv/shortcuts/clojure/tertiary-S-m)))
    "s-n" (argless (if vemv/shortcuts/clojure/tertiary-n (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-n vemv/shortcuts/clojure/tertiary-n)))
    "s-S-n" (argless (if vemv/shortcuts/clojure/tertiary-S-n (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-n vemv/shortcuts/clojure/tertiary-S-n)))
    "s-o" (argless (if vemv/shortcuts/clojure/tertiary-o (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-o vemv/shortcuts/clojure/tertiary-o)))
    "s-S-o" (argless (if vemv/shortcuts/clojure/tertiary-S-o (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-o vemv/shortcuts/clojure/tertiary-S-o)))
    "s-p" (argless (if vemv/shortcuts/clojure/tertiary-p (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-p vemv/shortcuts/clojure/tertiary-p)))
    "s-S-p" (argless (if vemv/shortcuts/clojure/tertiary-S-p (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-p vemv/shortcuts/clojure/tertiary-S-p)))
    "s-q" (argless (if vemv/shortcuts/clojure/tertiary-q (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-q vemv/shortcuts/clojure/tertiary-q)))
    "s-S-q" (argless (if vemv/shortcuts/clojure/tertiary-S-q (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-q vemv/shortcuts/clojure/tertiary-S-q)))
    "s-r" (argless (if vemv/shortcuts/clojure/tertiary-r (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-r vemv/shortcuts/clojure/tertiary-r)))
    "s-S-r" (argless (if vemv/shortcuts/clojure/tertiary-S-r (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-r vemv/shortcuts/clojure/tertiary-S-r)))
    "s-s" (argless (if vemv/shortcuts/clojure/tertiary-s (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-s vemv/shortcuts/clojure/tertiary-s)))
    "s-S-s" (argless (if vemv/shortcuts/clojure/tertiary-S-s (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-s vemv/shortcuts/clojure/tertiary-S-s)))
    "s-t" (argless (if vemv/shortcuts/clojure/tertiary-t (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-t vemv/shortcuts/clojure/tertiary-t)))
    "s-S-t" (argless (if vemv/shortcuts/clojure/tertiary-S-t (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-t vemv/shortcuts/clojure/tertiary-S-t)))
    "s-u" (argless (if vemv/shortcuts/clojure/tertiary-u (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-u vemv/shortcuts/clojure/tertiary-u)))
    "s-S-u" (argless (if vemv/shortcuts/clojure/tertiary-S-u (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-u vemv/shortcuts/clojure/tertiary-S-u)))
    "s-v" (argless (if vemv/shortcuts/clojure/tertiary-v (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-v vemv/shortcuts/clojure/tertiary-v)))
    "s-S-v" (argless (if vemv/shortcuts/clojure/tertiary-S-v (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-v vemv/shortcuts/clojure/tertiary-S-v)))
    "s-w" (argless (if vemv/shortcuts/clojure/tertiary-w (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-w vemv/shortcuts/clojure/tertiary-w)))
    "s-S-w" (argless (if vemv/shortcuts/clojure/tertiary-S-w (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-w vemv/shortcuts/clojure/tertiary-S-w)))
    "s-x" (argless (if vemv/shortcuts/clojure/tertiary-x (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-x vemv/shortcuts/clojure/tertiary-x)))
    "s-S-x" (argless (if vemv/shortcuts/clojure/tertiary-S-x (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-x vemv/shortcuts/clojure/tertiary-S-x)))
    "s-y" (argless (if vemv/shortcuts/clojure/tertiary-y (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-y vemv/shortcuts/clojure/tertiary-y)))
    "s-S-y" (argless (if vemv/shortcuts/clojure/tertiary-S-y (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-y vemv/shortcuts/clojure/tertiary-S-y)))
    "s-z" (argless (if vemv/shortcuts/clojure/tertiary-z (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-z vemv/shortcuts/clojure/tertiary-z)))
    "s-S-z" (argless (if vemv/shortcuts/clojure/tertiary-S-z (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-z vemv/shortcuts/clojure/tertiary-S-z)))
    "s-0" (argless (if vemv/shortcuts/clojure/tertiary-0 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-0 vemv/shortcuts/clojure/tertiary-0)))
    "s-S-0" (argless (if vemv/shortcuts/clojure/tertiary-S-0 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-0 vemv/shortcuts/clojure/tertiary-S-0)))
    "s-1" (argless (if vemv/shortcuts/clojure/tertiary-1 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-1 vemv/shortcuts/clojure/tertiary-1)))
    "s-S-1" (argless (if vemv/shortcuts/clojure/tertiary-S-1 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-1 vemv/shortcuts/clojure/tertiary-S-1)))
    "s-2" (argless (if vemv/shortcuts/clojure/tertiary-2 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-2 vemv/shortcuts/clojure/tertiary-2)))
    "s-S-2" (argless (if vemv/shortcuts/clojure/tertiary-S-2 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-2 vemv/shortcuts/clojure/tertiary-S-2)))
    "s-3" (argless (if vemv/shortcuts/clojure/tertiary-3 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-3 vemv/shortcuts/clojure/tertiary-3)))
    "s-S-3" (argless (if vemv/shortcuts/clojure/tertiary-S-3 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-3 vemv/shortcuts/clojure/tertiary-S-3)))
    "s-4" (argless (if vemv/shortcuts/clojure/tertiary-4 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-4 vemv/shortcuts/clojure/tertiary-4)))
    "s-S-4" (argless (if vemv/shortcuts/clojure/tertiary-S-4 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-4 vemv/shortcuts/clojure/tertiary-S-4)))
    "s-5" (argless (if vemv/shortcuts/clojure/tertiary-5 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-5 vemv/shortcuts/clojure/tertiary-5)))
    "s-S-5" (argless (if vemv/shortcuts/clojure/tertiary-S-5 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-5 vemv/shortcuts/clojure/tertiary-S-5)))
    "s-6" (argless (if vemv/shortcuts/clojure/tertiary-6 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-6 vemv/shortcuts/clojure/tertiary-6)))
    "s-S-6" (argless (if vemv/shortcuts/clojure/tertiary-S-6 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-6 vemv/shortcuts/clojure/tertiary-S-6)))
    "s-7" (argless (if vemv/shortcuts/clojure/tertiary-7 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-7 vemv/shortcuts/clojure/tertiary-7)))
    "s-S-7" (argless (if vemv/shortcuts/clojure/tertiary-S-7 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-7 vemv/shortcuts/clojure/tertiary-S-7)))
    "s-8" (argless (if vemv/shortcuts/clojure/tertiary-8 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-8 vemv/shortcuts/clojure/tertiary-8)))
    "s-S-8" (argless (if vemv/shortcuts/clojure/tertiary-S-8 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-8 vemv/shortcuts/clojure/tertiary-S-8)))
    "s-9" (argless (if vemv/shortcuts/clojure/tertiary-9 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-9 vemv/shortcuts/clojure/tertiary-9)))
    "s-S-9" (argless (if vemv/shortcuts/clojure/tertiary-S-9 (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-9 vemv/shortcuts/clojure/tertiary-S-9)))
    "s-!" (argless (if vemv/shortcuts/clojure/tertiary-bang (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-bang vemv/shortcuts/clojure/tertiary-bang)))
    "s-S-!" (argless (if vemv/shortcuts/clojure/tertiary-S-bang (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-bang vemv/shortcuts/clojure/tertiary-S-bang)))
    "s-@" (argless (if vemv/shortcuts/clojure/tertiary-at (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-at vemv/shortcuts/clojure/tertiary-at)))
    "s-S-@" (argless (if vemv/shortcuts/clojure/tertiary-S-at (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-at vemv/shortcuts/clojure/tertiary-S-at)))
    "s-&" (argless (if vemv/shortcuts/clojure/tertiary-ampersand (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-ampersand vemv/shortcuts/clojure/tertiary-ampersand)))
    "s-S-&" (argless (if vemv/shortcuts/clojure/tertiary-S-ampersand (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-ampersand vemv/shortcuts/clojure/tertiary-S-ampersand)))
    "s-#" (argless (if vemv/shortcuts/clojure/tertiary-hash (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-hash vemv/shortcuts/clojure/tertiary-hash)))
    "s-S-#" (argless (if vemv/shortcuts/clojure/tertiary-S-hash (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-hash vemv/shortcuts/clojure/tertiary-S-hash)))
    "s-%" (argless (if vemv/shortcuts/clojure/tertiary-percent (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-percent vemv/shortcuts/clojure/tertiary-percent)))
    "s-S-%" (argless (if vemv/shortcuts/clojure/tertiary-S-percent (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-percent vemv/shortcuts/clojure/tertiary-S-percent)))
    "s-^" (argless (if vemv/shortcuts/clojure/tertiary-caret (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-caret vemv/shortcuts/clojure/tertiary-caret)))
    "s-S-^" (argless (if vemv/shortcuts/clojure/tertiary-S-caret (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-caret vemv/shortcuts/clojure/tertiary-S-caret)))
    "s-~" (argless (if vemv/shortcuts/clojure/tertiary-tilde (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-tilde vemv/shortcuts/clojure/tertiary-tilde)))
    "s-S-~" (argless (if vemv/shortcuts/clojure/tertiary-S-tilde (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-tilde vemv/shortcuts/clojure/tertiary-S-tilde)))
    "s-$" (argless (if vemv/shortcuts/clojure/tertiary-dollar (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-dollar vemv/shortcuts/clojure/tertiary-dollar)))
    "s-S-$" (argless (if vemv/shortcuts/clojure/tertiary-S-dollar (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-dollar vemv/shortcuts/clojure/tertiary-S-dollar)))
    "s-_" (argless (if vemv/shortcuts/clojure/tertiary-underscore (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-underscore vemv/shortcuts/clojure/tertiary-underscore)))
    "s-S-_" (argless (if vemv/shortcuts/clojure/tertiary-S-underscore (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-underscore vemv/shortcuts/clojure/tertiary-S-underscore)))
    "s--" (argless (if vemv/shortcuts/clojure/tertiary-dash (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-dash vemv/shortcuts/clojure/tertiary-dash)))
    "s-S--" (argless (if vemv/shortcuts/clojure/tertiary-S-dash (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-dash vemv/shortcuts/clojure/tertiary-S-dash)))
    "s-," (argless (if vemv/shortcuts/clojure/tertiary-comma (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-comma vemv/shortcuts/clojure/tertiary-comma)))
    "s-S-," (argless (if vemv/shortcuts/clojure/tertiary-S-comma (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-comma vemv/shortcuts/clojure/tertiary-S-comma)))
    "s-;" (argless (if vemv/shortcuts/clojure/tertiary-semicolon (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-semicolon vemv/shortcuts/clojure/tertiary-semicolon)))
    "s-S-;" (argless (if vemv/shortcuts/clojure/tertiary-S-semicolon (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-semicolon vemv/shortcuts/clojure/tertiary-S-semicolon)))
    "s-:" (argless (if vemv/shortcuts/clojure/tertiary-colon (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-colon vemv/shortcuts/clojure/tertiary-colon)))
    "s-S-:" (argless (if vemv/shortcuts/clojure/tertiary-S-colon (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-colon vemv/shortcuts/clojure/tertiary-S-colon)))
    "s-?" (argless (if vemv/shortcuts/clojure/tertiary-question-mark (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-question-mark vemv/shortcuts/clojure/tertiary-question-mark)))
    "s-S-?" (argless (if vemv/shortcuts/clojure/tertiary-S-question-mark (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-question-mark vemv/shortcuts/clojure/tertiary-S-question-mark)))
    "s-." (argless (if vemv/shortcuts/clojure/tertiary-dot (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-dot vemv/shortcuts/clojure/tertiary-dot)))
    "s-S-." (argless (if vemv/shortcuts/clojure/tertiary-S-dot (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-dot vemv/shortcuts/clojure/tertiary-S-dot)))
    "s-'" (argless (if vemv/shortcuts/clojure/tertiary-single-quote (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-single-quote vemv/shortcuts/clojure/tertiary-single-quote)))
    "s-S-'" (argless (if vemv/shortcuts/clojure/tertiary-S-single-quote (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-single-quote vemv/shortcuts/clojure/tertiary-S-single-quote)))
    "s-(" (argless (if vemv/shortcuts/clojure/tertiary-left-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-left-parens vemv/shortcuts/clojure/tertiary-left-parens)))
    "s-S-(" (argless (if vemv/shortcuts/clojure/tertiary-S-left-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-left-parens vemv/shortcuts/clojure/tertiary-S-left-parens)))
    "s-)" (argless (if vemv/shortcuts/clojure/tertiary-right-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-right-parens vemv/shortcuts/clojure/tertiary-right-parens)))
    "s-S-)" (argless (if vemv/shortcuts/clojure/tertiary-S-right-parens (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-right-parens vemv/shortcuts/clojure/tertiary-S-right-parens)))
    "s-[" (argless (if vemv/shortcuts/clojure/tertiary-left-bracket (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-left-bracket vemv/shortcuts/clojure/tertiary-left-bracket)))
    "s-S-[" (argless (if vemv/shortcuts/clojure/tertiary-S-left-bracket (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-left-bracket vemv/shortcuts/clojure/tertiary-S-left-bracket)))
    "s-]" (argless (if vemv/shortcuts/clojure/tertiary-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-right-bracket vemv/shortcuts/clojure/tertiary-right-bracket)))
    "s-S-]" (argless (if vemv/shortcuts/clojure/tertiary-S-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-right-bracket vemv/shortcuts/clojure/tertiary-S-right-bracket)))
    "s-{" (argless (if vemv/shortcuts/clojure/tertiary-left-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-left-curly vemv/shortcuts/clojure/tertiary-left-curly)))
    "s-S-{" (argless (if vemv/shortcuts/clojure/tertiary-S-left-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-left-curly vemv/shortcuts/clojure/tertiary-S-left-curly)))
    "s-}" (argless (if vemv/shortcuts/clojure/tertiary-right-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-right-curly vemv/shortcuts/clojure/tertiary-right-curly)))
    "s-S-}" (argless (if vemv/shortcuts/clojure/tertiary-S-right-curly (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-right-curly vemv/shortcuts/clojure/tertiary-S-right-curly)))
    "s-*" (argless (if vemv/shortcuts/clojure/tertiary-star (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-star vemv/shortcuts/clojure/tertiary-star)))
    "s-S-*" (argless (if vemv/shortcuts/clojure/tertiary-S-star (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-star vemv/shortcuts/clojure/tertiary-S-star)))
    "s-/" (argless (if vemv/shortcuts/clojure/tertiary-slash (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-slash vemv/shortcuts/clojure/tertiary-slash)))
    "s-S-/" (argless (if vemv/shortcuts/clojure/tertiary-S-slash (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-slash vemv/shortcuts/clojure/tertiary-S-slash)))
    "s-`" (argless (if vemv/shortcuts/clojure/tertiary-backtick (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-backtick vemv/shortcuts/clojure/tertiary-backtick)))
    "s-S-`" (argless (if vemv/shortcuts/clojure/tertiary-S-backtick (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-backtick vemv/shortcuts/clojure/tertiary-S-backtick)))
    "s-+" (argless (if vemv/shortcuts/clojure/tertiary-plus (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-plus vemv/shortcuts/clojure/tertiary-plus)))
    "s-S-+" (argless (if vemv/shortcuts/clojure/tertiary-S-plus (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-plus vemv/shortcuts/clojure/tertiary-S-plus)))
    "s-<backspace>" (argless (if vemv/shortcuts/clojure/tertiary-backspace (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-backspace vemv/shortcuts/clojure/tertiary-backspace)))
    "s-S-<backspace>" (argless (if vemv/shortcuts/clojure/tertiary-S-backspace (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-backspace vemv/shortcuts/clojure/tertiary-S-backspace)))
    "s-<down>" (argless (if vemv/shortcuts/clojure/tertiary-down (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-down vemv/shortcuts/clojure/tertiary-down)))
    "s-S-<down>" (argless (if vemv/shortcuts/clojure/tertiary-S-down (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-down vemv/shortcuts/clojure/tertiary-S-down)))
    "s-<end>" (argless (if vemv/shortcuts/clojure/tertiary-end (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-end vemv/shortcuts/clojure/tertiary-end)))
    "s-S-<end>" (argless (if vemv/shortcuts/clojure/tertiary-S-end (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-end vemv/shortcuts/clojure/tertiary-S-end)))
    "s-<home>" (argless (if vemv/shortcuts/clojure/tertiary-home (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-home vemv/shortcuts/clojure/tertiary-home)))
    "s-S-<home>" (argless (if vemv/shortcuts/clojure/tertiary-S-home (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-home vemv/shortcuts/clojure/tertiary-S-home)))
    "s-<left>" (argless (if vemv/shortcuts/clojure/tertiary-left (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-left vemv/shortcuts/clojure/tertiary-left)))
    "s-S-<left>" (argless (if vemv/shortcuts/clojure/tertiary-S-left (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-left vemv/shortcuts/clojure/tertiary-S-left)))
    "s-<next>" (argless (if vemv/shortcuts/clojure/tertiary-next (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-next vemv/shortcuts/clojure/tertiary-next)))
    "s-S-<next>" (argless (if vemv/shortcuts/clojure/tertiary-S-next (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-next vemv/shortcuts/clojure/tertiary-S-next)))
    "s-<prior>" (argless (if vemv/shortcuts/clojure/tertiary-prior (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-prior vemv/shortcuts/clojure/tertiary-prior)))
    "s-S-<prior>" (argless (if vemv/shortcuts/clojure/tertiary-S-prior (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-prior vemv/shortcuts/clojure/tertiary-S-prior)))
    "s-<right>" (argless (if vemv/shortcuts/clojure/tertiary-right (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-right vemv/shortcuts/clojure/tertiary-right)))
    "s-S-<right>" (argless (if vemv/shortcuts/clojure/tertiary-S-right (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-right vemv/shortcuts/clojure/tertiary-S-right)))
    "s-<up>" (argless (if vemv/shortcuts/clojure/tertiary-up (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-up vemv/shortcuts/clojure/tertiary-up)))
    "s-S-<up>" (argless (if vemv/shortcuts/clojure/tertiary-S-up (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-up vemv/shortcuts/clojure/tertiary-S-up)))
    "s-=" (argless (if vemv/shortcuts/clojure/tertiary-equal (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-equal vemv/shortcuts/clojure/tertiary-equal)))
    "s-S-=" (argless (if vemv/shortcuts/clojure/tertiary-S-equal (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-equal vemv/shortcuts/clojure/tertiary-S-equal)))
    "s-|" (argless (if vemv/shortcuts/clojure/tertiary-bar (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-bar vemv/shortcuts/clojure/tertiary-bar)))
    "s-S-|" (argless (if vemv/shortcuts/clojure/tertiary-S-bar (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-bar vemv/shortcuts/clojure/tertiary-S-bar)))
    "s-RET" (argless (if vemv/shortcuts/clojure/tertiary-RET (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-RET vemv/shortcuts/clojure/tertiary-RET)))
    "s-S-RET" (argless (if vemv/shortcuts/clojure/tertiary-S-RET (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-RET vemv/shortcuts/clojure/tertiary-S-RET)))
    "s-SPC" (argless (if vemv/shortcuts/clojure/tertiary-SPC (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-SPC vemv/shortcuts/clojure/tertiary-SPC)))
    "s-S-SPC" (argless (if vemv/shortcuts/clojure/tertiary-S-SPC (vemv/keyboard-funcall :vemv/shortcuts/clojure/tertiary-S-SPC vemv/shortcuts/clojure/tertiary-S-SPC)))
))

(setq vemv/ruby-key-bindings
  (vemv/hash-map
    [f1] (argless (if vemv/shortcuts/ruby/f1 (vemv/keyboard-funcall :vemv/shortcuts/ruby/f1 vemv/shortcuts/ruby/f1)))
    "<s-f1>" (argless (if vemv/shortcuts/ruby/S-f1 (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-f1 vemv/shortcuts/ruby/S-f1)))
    [f10] (argless (if vemv/shortcuts/ruby/f10 (vemv/keyboard-funcall :vemv/shortcuts/ruby/f10 vemv/shortcuts/ruby/f10)))
    "<s-f10>" (argless (if vemv/shortcuts/ruby/S-f10 (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-f10 vemv/shortcuts/ruby/S-f10)))
    [f11] (argless (if vemv/shortcuts/ruby/f11 (vemv/keyboard-funcall :vemv/shortcuts/ruby/f11 vemv/shortcuts/ruby/f11)))
    "<s-f11>" (argless (if vemv/shortcuts/ruby/S-f11 (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-f11 vemv/shortcuts/ruby/S-f11)))
    [f12] (argless (if vemv/shortcuts/ruby/f12 (vemv/keyboard-funcall :vemv/shortcuts/ruby/f12 vemv/shortcuts/ruby/f12)))
    "<s-f12>" (argless (if vemv/shortcuts/ruby/S-f12 (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-f12 vemv/shortcuts/ruby/S-f12)))
    [f2] (argless (if vemv/shortcuts/ruby/f2 (vemv/keyboard-funcall :vemv/shortcuts/ruby/f2 vemv/shortcuts/ruby/f2)))
    "<s-f2>" (argless (if vemv/shortcuts/ruby/S-f2 (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-f2 vemv/shortcuts/ruby/S-f2)))
    [f3] (argless (if vemv/shortcuts/ruby/f3 (vemv/keyboard-funcall :vemv/shortcuts/ruby/f3 vemv/shortcuts/ruby/f3)))
    "<s-f3>" (argless (if vemv/shortcuts/ruby/S-f3 (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-f3 vemv/shortcuts/ruby/S-f3)))
    [f4] (argless (if vemv/shortcuts/ruby/f4 (vemv/keyboard-funcall :vemv/shortcuts/ruby/f4 vemv/shortcuts/ruby/f4)))
    "<s-f4>" (argless (if vemv/shortcuts/ruby/S-f4 (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-f4 vemv/shortcuts/ruby/S-f4)))
    [f5] (argless (if vemv/shortcuts/ruby/f5 (vemv/keyboard-funcall :vemv/shortcuts/ruby/f5 vemv/shortcuts/ruby/f5)))
    "<s-f5>" (argless (if vemv/shortcuts/ruby/S-f5 (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-f5 vemv/shortcuts/ruby/S-f5)))
    [f6] (argless (if vemv/shortcuts/ruby/f6 (vemv/keyboard-funcall :vemv/shortcuts/ruby/f6 vemv/shortcuts/ruby/f6)))
    "<s-f6>" (argless (if vemv/shortcuts/ruby/S-f6 (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-f6 vemv/shortcuts/ruby/S-f6)))
    [f7] (argless (if vemv/shortcuts/ruby/f7 (vemv/keyboard-funcall :vemv/shortcuts/ruby/f7 vemv/shortcuts/ruby/f7)))
    "<s-f7>" (argless (if vemv/shortcuts/ruby/S-f7 (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-f7 vemv/shortcuts/ruby/S-f7)))
    [f8] (argless (if vemv/shortcuts/ruby/f8 (vemv/keyboard-funcall :vemv/shortcuts/ruby/f8 vemv/shortcuts/ruby/f8)))
    "<s-f8>" (argless (if vemv/shortcuts/ruby/S-f8 (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-f8 vemv/shortcuts/ruby/S-f8)))
    [f9] (argless (if vemv/shortcuts/ruby/f9 (vemv/keyboard-funcall :vemv/shortcuts/ruby/f9 vemv/shortcuts/ruby/f9)))
    "<s-f9>" (argless (if vemv/shortcuts/ruby/S-f9 (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-f9 vemv/shortcuts/ruby/S-f9)))
    "<backspace>" (argless (if vemv/shortcuts/ruby/backspace (vemv/keyboard-funcall :vemv/shortcuts/ruby/backspace vemv/shortcuts/ruby/backspace)))
    "S-<backspace>" (argless (if vemv/shortcuts/ruby/S-backspace (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-backspace vemv/shortcuts/ruby/S-backspace)))
    "<down>" (argless (if vemv/shortcuts/ruby/down (vemv/keyboard-funcall :vemv/shortcuts/ruby/down vemv/shortcuts/ruby/down)))
    "S-<down>" (argless (if vemv/shortcuts/ruby/S-down (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-down vemv/shortcuts/ruby/S-down)))
    "<end>" (argless (if vemv/shortcuts/ruby/end (vemv/keyboard-funcall :vemv/shortcuts/ruby/end vemv/shortcuts/ruby/end)))
    "S-<end>" (argless (if vemv/shortcuts/ruby/S-end (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-end vemv/shortcuts/ruby/S-end)))
    "<home>" (argless (if vemv/shortcuts/ruby/home (vemv/keyboard-funcall :vemv/shortcuts/ruby/home vemv/shortcuts/ruby/home)))
    "S-<home>" (argless (if vemv/shortcuts/ruby/S-home (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-home vemv/shortcuts/ruby/S-home)))
    "<left>" (argless (if vemv/shortcuts/ruby/left (vemv/keyboard-funcall :vemv/shortcuts/ruby/left vemv/shortcuts/ruby/left)))
    "S-<left>" (argless (if vemv/shortcuts/ruby/S-left (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-left vemv/shortcuts/ruby/S-left)))
    "<next>" (argless (if vemv/shortcuts/ruby/next (vemv/keyboard-funcall :vemv/shortcuts/ruby/next vemv/shortcuts/ruby/next)))
    "S-<next>" (argless (if vemv/shortcuts/ruby/S-next (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-next vemv/shortcuts/ruby/S-next)))
    "<prior>" (argless (if vemv/shortcuts/ruby/prior (vemv/keyboard-funcall :vemv/shortcuts/ruby/prior vemv/shortcuts/ruby/prior)))
    "S-<prior>" (argless (if vemv/shortcuts/ruby/S-prior (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-prior vemv/shortcuts/ruby/S-prior)))
    "<right>" (argless (if vemv/shortcuts/ruby/right (vemv/keyboard-funcall :vemv/shortcuts/ruby/right vemv/shortcuts/ruby/right)))
    "S-<right>" (argless (if vemv/shortcuts/ruby/S-right (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-right vemv/shortcuts/ruby/S-right)))
    "<up>" (argless (if vemv/shortcuts/ruby/up (vemv/keyboard-funcall :vemv/shortcuts/ruby/up vemv/shortcuts/ruby/up)))
    "S-<up>" (argless (if vemv/shortcuts/ruby/S-up (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-up vemv/shortcuts/ruby/S-up)))
    "RET" (argless (if vemv/shortcuts/ruby/RET (vemv/keyboard-funcall :vemv/shortcuts/ruby/RET vemv/shortcuts/ruby/RET)))
    "S-RET" (argless (if vemv/shortcuts/ruby/S-RET (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-RET vemv/shortcuts/ruby/S-RET)))
    "S-SPC" (argless (if vemv/shortcuts/ruby/S-SPC (vemv/keyboard-funcall :vemv/shortcuts/ruby/S-SPC vemv/shortcuts/ruby/S-SPC)))
    "C-a" (argless (if vemv/shortcuts/ruby/primary-a (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-a vemv/shortcuts/ruby/primary-a)))
    "C-M-a" (argless (if vemv/shortcuts/ruby/primary-secondary-a (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-a vemv/shortcuts/ruby/primary-secondary-a)))
    "C-S-a" (argless (if vemv/shortcuts/ruby/primary-S-a (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-a vemv/shortcuts/ruby/primary-S-a)))
    "C-b" (argless (if vemv/shortcuts/ruby/primary-b (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-b vemv/shortcuts/ruby/primary-b)))
    "C-M-b" (argless (if vemv/shortcuts/ruby/primary-secondary-b (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-b vemv/shortcuts/ruby/primary-secondary-b)))
    "C-S-b" (argless (if vemv/shortcuts/ruby/primary-S-b (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-b vemv/shortcuts/ruby/primary-S-b)))
    "C-c" (argless (if vemv/shortcuts/ruby/primary-c (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-c vemv/shortcuts/ruby/primary-c)))
    "C-M-c" (argless (if vemv/shortcuts/ruby/primary-secondary-c (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-c vemv/shortcuts/ruby/primary-secondary-c)))
    "C-S-c" (argless (if vemv/shortcuts/ruby/primary-S-c (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-c vemv/shortcuts/ruby/primary-S-c)))
    "C-d" (argless (if vemv/shortcuts/ruby/primary-d (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-d vemv/shortcuts/ruby/primary-d)))
    "C-M-d" (argless (if vemv/shortcuts/ruby/primary-secondary-d (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-d vemv/shortcuts/ruby/primary-secondary-d)))
    "C-S-d" (argless (if vemv/shortcuts/ruby/primary-S-d (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-d vemv/shortcuts/ruby/primary-S-d)))
    "C-e" (argless (if vemv/shortcuts/ruby/primary-e (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-e vemv/shortcuts/ruby/primary-e)))
    "C-M-e" (argless (if vemv/shortcuts/ruby/primary-secondary-e (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-e vemv/shortcuts/ruby/primary-secondary-e)))
    "C-S-e" (argless (if vemv/shortcuts/ruby/primary-S-e (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-e vemv/shortcuts/ruby/primary-S-e)))
    "C-f" (argless (if vemv/shortcuts/ruby/primary-f (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-f vemv/shortcuts/ruby/primary-f)))
    "C-M-f" (argless (if vemv/shortcuts/ruby/primary-secondary-f (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-f vemv/shortcuts/ruby/primary-secondary-f)))
    "C-S-f" (argless (if vemv/shortcuts/ruby/primary-S-f (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-f vemv/shortcuts/ruby/primary-S-f)))
    "C-h" (argless (if vemv/shortcuts/ruby/primary-h (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-h vemv/shortcuts/ruby/primary-h)))
    "C-M-h" (argless (if vemv/shortcuts/ruby/primary-secondary-h (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-h vemv/shortcuts/ruby/primary-secondary-h)))
    "C-S-h" (argless (if vemv/shortcuts/ruby/primary-S-h (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-h vemv/shortcuts/ruby/primary-S-h)))
    "C-j" (argless (if vemv/shortcuts/ruby/primary-j (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-j vemv/shortcuts/ruby/primary-j)))
    "C-M-j" (argless (if vemv/shortcuts/ruby/primary-secondary-j (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-j vemv/shortcuts/ruby/primary-secondary-j)))
    "C-S-j" (argless (if vemv/shortcuts/ruby/primary-S-j (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-j vemv/shortcuts/ruby/primary-S-j)))
    "C-k" (argless (if vemv/shortcuts/ruby/primary-k (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-k vemv/shortcuts/ruby/primary-k)))
    "C-M-k" (argless (if vemv/shortcuts/ruby/primary-secondary-k (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-k vemv/shortcuts/ruby/primary-secondary-k)))
    "C-S-k" (argless (if vemv/shortcuts/ruby/primary-S-k (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-k vemv/shortcuts/ruby/primary-S-k)))
    "C-l" (argless (if vemv/shortcuts/ruby/primary-l (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-l vemv/shortcuts/ruby/primary-l)))
    "C-M-l" (argless (if vemv/shortcuts/ruby/primary-secondary-l (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-l vemv/shortcuts/ruby/primary-secondary-l)))
    "C-S-l" (argless (if vemv/shortcuts/ruby/primary-S-l (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-l vemv/shortcuts/ruby/primary-S-l)))
    "C-n" (argless (if vemv/shortcuts/ruby/primary-n (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-n vemv/shortcuts/ruby/primary-n)))
    "C-M-n" (argless (if vemv/shortcuts/ruby/primary-secondary-n (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-n vemv/shortcuts/ruby/primary-secondary-n)))
    "C-S-n" (argless (if vemv/shortcuts/ruby/primary-S-n (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-n vemv/shortcuts/ruby/primary-S-n)))
    "C-o" (argless (if vemv/shortcuts/ruby/primary-o (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-o vemv/shortcuts/ruby/primary-o)))
    "C-M-o" (argless (if vemv/shortcuts/ruby/primary-secondary-o (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-o vemv/shortcuts/ruby/primary-secondary-o)))
    "C-S-o" (argless (if vemv/shortcuts/ruby/primary-S-o (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-o vemv/shortcuts/ruby/primary-S-o)))
    "C-p" (argless (if vemv/shortcuts/ruby/primary-p (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-p vemv/shortcuts/ruby/primary-p)))
    "C-M-p" (argless (if vemv/shortcuts/ruby/primary-secondary-p (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-p vemv/shortcuts/ruby/primary-secondary-p)))
    "C-S-p" (argless (if vemv/shortcuts/ruby/primary-S-p (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-p vemv/shortcuts/ruby/primary-S-p)))
    "C-q" (argless (if vemv/shortcuts/ruby/primary-q (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-q vemv/shortcuts/ruby/primary-q)))
    "C-M-q" (argless (if vemv/shortcuts/ruby/primary-secondary-q (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-q vemv/shortcuts/ruby/primary-secondary-q)))
    "C-S-q" (argless (if vemv/shortcuts/ruby/primary-S-q (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-q vemv/shortcuts/ruby/primary-S-q)))
    "C-r" (argless (if vemv/shortcuts/ruby/primary-r (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-r vemv/shortcuts/ruby/primary-r)))
    "C-M-r" (argless (if vemv/shortcuts/ruby/primary-secondary-r (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-r vemv/shortcuts/ruby/primary-secondary-r)))
    "C-S-r" (argless (if vemv/shortcuts/ruby/primary-S-r (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-r vemv/shortcuts/ruby/primary-S-r)))
    "C-s" (argless (if vemv/shortcuts/ruby/primary-s (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-s vemv/shortcuts/ruby/primary-s)))
    "C-M-s" (argless (if vemv/shortcuts/ruby/primary-secondary-s (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-s vemv/shortcuts/ruby/primary-secondary-s)))
    "C-S-s" (argless (if vemv/shortcuts/ruby/primary-S-s (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-s vemv/shortcuts/ruby/primary-S-s)))
    "C-t" (argless (if vemv/shortcuts/ruby/primary-t (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-t vemv/shortcuts/ruby/primary-t)))
    "C-M-t" (argless (if vemv/shortcuts/ruby/primary-secondary-t (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-t vemv/shortcuts/ruby/primary-secondary-t)))
    "C-S-t" (argless (if vemv/shortcuts/ruby/primary-S-t (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-t vemv/shortcuts/ruby/primary-S-t)))
    "C-u" (argless (if vemv/shortcuts/ruby/primary-u (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-u vemv/shortcuts/ruby/primary-u)))
    "C-M-u" (argless (if vemv/shortcuts/ruby/primary-secondary-u (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-u vemv/shortcuts/ruby/primary-secondary-u)))
    "C-S-u" (argless (if vemv/shortcuts/ruby/primary-S-u (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-u vemv/shortcuts/ruby/primary-S-u)))
    "C-v" (argless (if vemv/shortcuts/ruby/primary-v (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-v vemv/shortcuts/ruby/primary-v)))
    "C-M-v" (argless (if vemv/shortcuts/ruby/primary-secondary-v (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-v vemv/shortcuts/ruby/primary-secondary-v)))
    "C-S-v" (argless (if vemv/shortcuts/ruby/primary-S-v (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-v vemv/shortcuts/ruby/primary-S-v)))
    "C-w" (argless (if vemv/shortcuts/ruby/primary-w (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-w vemv/shortcuts/ruby/primary-w)))
    "C-M-w" (argless (if vemv/shortcuts/ruby/primary-secondary-w (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-w vemv/shortcuts/ruby/primary-secondary-w)))
    "C-S-w" (argless (if vemv/shortcuts/ruby/primary-S-w (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-w vemv/shortcuts/ruby/primary-S-w)))
    "C-x" (argless (if vemv/shortcuts/ruby/primary-x (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-x vemv/shortcuts/ruby/primary-x)))
    "C-M-x" (argless (if vemv/shortcuts/ruby/primary-secondary-x (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-x vemv/shortcuts/ruby/primary-secondary-x)))
    "C-S-x" (argless (if vemv/shortcuts/ruby/primary-S-x (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-x vemv/shortcuts/ruby/primary-S-x)))
    "C-y" (argless (if vemv/shortcuts/ruby/primary-y (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-y vemv/shortcuts/ruby/primary-y)))
    "C-M-y" (argless (if vemv/shortcuts/ruby/primary-secondary-y (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-y vemv/shortcuts/ruby/primary-secondary-y)))
    "C-S-y" (argless (if vemv/shortcuts/ruby/primary-S-y (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-y vemv/shortcuts/ruby/primary-S-y)))
    "C-z" (argless (if vemv/shortcuts/ruby/primary-z (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-z vemv/shortcuts/ruby/primary-z)))
    "C-M-z" (argless (if vemv/shortcuts/ruby/primary-secondary-z (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-z vemv/shortcuts/ruby/primary-secondary-z)))
    "C-S-z" (argless (if vemv/shortcuts/ruby/primary-S-z (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-z vemv/shortcuts/ruby/primary-S-z)))
    "C-1" (argless (if vemv/shortcuts/ruby/primary-1 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-1 vemv/shortcuts/ruby/primary-1)))
    "C-M-1" (argless (if vemv/shortcuts/ruby/primary-secondary-1 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-1 vemv/shortcuts/ruby/primary-secondary-1)))
    "C-S-1" (argless (if vemv/shortcuts/ruby/primary-S-1 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-1 vemv/shortcuts/ruby/primary-S-1)))
    "C-2" (argless (if vemv/shortcuts/ruby/primary-2 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-2 vemv/shortcuts/ruby/primary-2)))
    "C-M-2" (argless (if vemv/shortcuts/ruby/primary-secondary-2 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-2 vemv/shortcuts/ruby/primary-secondary-2)))
    "C-S-2" (argless (if vemv/shortcuts/ruby/primary-S-2 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-2 vemv/shortcuts/ruby/primary-S-2)))
    "C-3" (argless (if vemv/shortcuts/ruby/primary-3 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-3 vemv/shortcuts/ruby/primary-3)))
    "C-M-3" (argless (if vemv/shortcuts/ruby/primary-secondary-3 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-3 vemv/shortcuts/ruby/primary-secondary-3)))
    "C-S-3" (argless (if vemv/shortcuts/ruby/primary-S-3 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-3 vemv/shortcuts/ruby/primary-S-3)))
    "C-4" (argless (if vemv/shortcuts/ruby/primary-4 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-4 vemv/shortcuts/ruby/primary-4)))
    "C-M-4" (argless (if vemv/shortcuts/ruby/primary-secondary-4 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-4 vemv/shortcuts/ruby/primary-secondary-4)))
    "C-S-4" (argless (if vemv/shortcuts/ruby/primary-S-4 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-4 vemv/shortcuts/ruby/primary-S-4)))
    "C-5" (argless (if vemv/shortcuts/ruby/primary-5 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-5 vemv/shortcuts/ruby/primary-5)))
    "C-M-5" (argless (if vemv/shortcuts/ruby/primary-secondary-5 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-5 vemv/shortcuts/ruby/primary-secondary-5)))
    "C-S-5" (argless (if vemv/shortcuts/ruby/primary-S-5 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-5 vemv/shortcuts/ruby/primary-S-5)))
    "C-6" (argless (if vemv/shortcuts/ruby/primary-6 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-6 vemv/shortcuts/ruby/primary-6)))
    "C-M-6" (argless (if vemv/shortcuts/ruby/primary-secondary-6 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-6 vemv/shortcuts/ruby/primary-secondary-6)))
    "C-S-6" (argless (if vemv/shortcuts/ruby/primary-S-6 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-6 vemv/shortcuts/ruby/primary-S-6)))
    "C-7" (argless (if vemv/shortcuts/ruby/primary-7 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-7 vemv/shortcuts/ruby/primary-7)))
    "C-M-7" (argless (if vemv/shortcuts/ruby/primary-secondary-7 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-7 vemv/shortcuts/ruby/primary-secondary-7)))
    "C-S-7" (argless (if vemv/shortcuts/ruby/primary-S-7 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-7 vemv/shortcuts/ruby/primary-S-7)))
    "C-8" (argless (if vemv/shortcuts/ruby/primary-8 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-8 vemv/shortcuts/ruby/primary-8)))
    "C-M-8" (argless (if vemv/shortcuts/ruby/primary-secondary-8 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-8 vemv/shortcuts/ruby/primary-secondary-8)))
    "C-S-8" (argless (if vemv/shortcuts/ruby/primary-S-8 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-8 vemv/shortcuts/ruby/primary-S-8)))
    "C-9" (argless (if vemv/shortcuts/ruby/primary-9 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-9 vemv/shortcuts/ruby/primary-9)))
    "C-M-9" (argless (if vemv/shortcuts/ruby/primary-secondary-9 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-9 vemv/shortcuts/ruby/primary-secondary-9)))
    "C-S-9" (argless (if vemv/shortcuts/ruby/primary-S-9 (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-9 vemv/shortcuts/ruby/primary-S-9)))
    "C-!" (argless (if vemv/shortcuts/ruby/primary-bang (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-bang vemv/shortcuts/ruby/primary-bang)))
    "C-M-!" (argless (if vemv/shortcuts/ruby/primary-secondary-bang (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-bang vemv/shortcuts/ruby/primary-secondary-bang)))
    "C-S-!" (argless (if vemv/shortcuts/ruby/primary-S-bang (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-bang vemv/shortcuts/ruby/primary-S-bang)))
    "C-@" (argless (if vemv/shortcuts/ruby/primary-at (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-at vemv/shortcuts/ruby/primary-at)))
    "C-M-@" (argless (if vemv/shortcuts/ruby/primary-secondary-at (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-at vemv/shortcuts/ruby/primary-secondary-at)))
    "C-S-@" (argless (if vemv/shortcuts/ruby/primary-S-at (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-at vemv/shortcuts/ruby/primary-S-at)))
    "C-&" (argless (if vemv/shortcuts/ruby/primary-ampersand (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-ampersand vemv/shortcuts/ruby/primary-ampersand)))
    "C-M-&" (argless (if vemv/shortcuts/ruby/primary-secondary-ampersand (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-ampersand vemv/shortcuts/ruby/primary-secondary-ampersand)))
    "C-S-&" (argless (if vemv/shortcuts/ruby/primary-S-ampersand (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-ampersand vemv/shortcuts/ruby/primary-S-ampersand)))
    "C-#" (argless (if vemv/shortcuts/ruby/primary-hash (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-hash vemv/shortcuts/ruby/primary-hash)))
    "C-M-#" (argless (if vemv/shortcuts/ruby/primary-secondary-hash (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-hash vemv/shortcuts/ruby/primary-secondary-hash)))
    "C-S-#" (argless (if vemv/shortcuts/ruby/primary-S-hash (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-hash vemv/shortcuts/ruby/primary-S-hash)))
    "C-%" (argless (if vemv/shortcuts/ruby/primary-percent (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-percent vemv/shortcuts/ruby/primary-percent)))
    "C-M-%" (argless (if vemv/shortcuts/ruby/primary-secondary-percent (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-percent vemv/shortcuts/ruby/primary-secondary-percent)))
    "C-S-%" (argless (if vemv/shortcuts/ruby/primary-S-percent (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-percent vemv/shortcuts/ruby/primary-S-percent)))
    "C-^" (argless (if vemv/shortcuts/ruby/primary-caret (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-caret vemv/shortcuts/ruby/primary-caret)))
    "C-M-^" (argless (if vemv/shortcuts/ruby/primary-secondary-caret (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-caret vemv/shortcuts/ruby/primary-secondary-caret)))
    "C-S-^" (argless (if vemv/shortcuts/ruby/primary-S-caret (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-caret vemv/shortcuts/ruby/primary-S-caret)))
    "C-$" (argless (if vemv/shortcuts/ruby/primary-dollar (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-dollar vemv/shortcuts/ruby/primary-dollar)))
    "C-M-$" (argless (if vemv/shortcuts/ruby/primary-secondary-dollar (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-dollar vemv/shortcuts/ruby/primary-secondary-dollar)))
    "C-S-$" (argless (if vemv/shortcuts/ruby/primary-S-dollar (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-dollar vemv/shortcuts/ruby/primary-S-dollar)))
    "C-_" (argless (if vemv/shortcuts/ruby/primary-underscore (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-underscore vemv/shortcuts/ruby/primary-underscore)))
    "C-M-_" (argless (if vemv/shortcuts/ruby/primary-secondary-underscore (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-underscore vemv/shortcuts/ruby/primary-secondary-underscore)))
    "C-S-_" (argless (if vemv/shortcuts/ruby/primary-S-underscore (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-underscore vemv/shortcuts/ruby/primary-S-underscore)))
    "C--" (argless (if vemv/shortcuts/ruby/primary-dash (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-dash vemv/shortcuts/ruby/primary-dash)))
    "C-M--" (argless (if vemv/shortcuts/ruby/primary-secondary-dash (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-dash vemv/shortcuts/ruby/primary-secondary-dash)))
    "C-S--" (argless (if vemv/shortcuts/ruby/primary-S-dash (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-dash vemv/shortcuts/ruby/primary-S-dash)))
    "C-," (argless (if vemv/shortcuts/ruby/primary-comma (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-comma vemv/shortcuts/ruby/primary-comma)))
    "C-M-," (argless (if vemv/shortcuts/ruby/primary-secondary-comma (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-comma vemv/shortcuts/ruby/primary-secondary-comma)))
    "C-S-," (argless (if vemv/shortcuts/ruby/primary-S-comma (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-comma vemv/shortcuts/ruby/primary-S-comma)))
    "C-;" (argless (if vemv/shortcuts/ruby/primary-semicolon (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-semicolon vemv/shortcuts/ruby/primary-semicolon)))
    "C-M-;" (argless (if vemv/shortcuts/ruby/primary-secondary-semicolon (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-semicolon vemv/shortcuts/ruby/primary-secondary-semicolon)))
    "C-S-;" (argless (if vemv/shortcuts/ruby/primary-S-semicolon (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-semicolon vemv/shortcuts/ruby/primary-S-semicolon)))
    "C-:" (argless (if vemv/shortcuts/ruby/primary-colon (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-colon vemv/shortcuts/ruby/primary-colon)))
    "C-M-:" (argless (if vemv/shortcuts/ruby/primary-secondary-colon (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-colon vemv/shortcuts/ruby/primary-secondary-colon)))
    "C-S-:" (argless (if vemv/shortcuts/ruby/primary-S-colon (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-colon vemv/shortcuts/ruby/primary-S-colon)))
    "C-?" (argless (if vemv/shortcuts/ruby/primary-question-mark (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-question-mark vemv/shortcuts/ruby/primary-question-mark)))
    "C-M-?" (argless (if vemv/shortcuts/ruby/primary-secondary-question-mark (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-question-mark vemv/shortcuts/ruby/primary-secondary-question-mark)))
    "C-S-?" (argless (if vemv/shortcuts/ruby/primary-S-question-mark (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-question-mark vemv/shortcuts/ruby/primary-S-question-mark)))
    "C-." (argless (if vemv/shortcuts/ruby/primary-dot (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-dot vemv/shortcuts/ruby/primary-dot)))
    "C-M-." (argless (if vemv/shortcuts/ruby/primary-secondary-dot (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-dot vemv/shortcuts/ruby/primary-secondary-dot)))
    "C-S-." (argless (if vemv/shortcuts/ruby/primary-S-dot (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-dot vemv/shortcuts/ruby/primary-S-dot)))
    "C-'" (argless (if vemv/shortcuts/ruby/primary-single-quote (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-single-quote vemv/shortcuts/ruby/primary-single-quote)))
    "C-M-'" (argless (if vemv/shortcuts/ruby/primary-secondary-single-quote (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-single-quote vemv/shortcuts/ruby/primary-secondary-single-quote)))
    "C-S-'" (argless (if vemv/shortcuts/ruby/primary-S-single-quote (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-single-quote vemv/shortcuts/ruby/primary-S-single-quote)))
    "C-(" (argless (if vemv/shortcuts/ruby/primary-left-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-left-parens vemv/shortcuts/ruby/primary-left-parens)))
    "C-M-(" (argless (if vemv/shortcuts/ruby/primary-secondary-left-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-left-parens vemv/shortcuts/ruby/primary-secondary-left-parens)))
    "C-S-(" (argless (if vemv/shortcuts/ruby/primary-S-left-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-left-parens vemv/shortcuts/ruby/primary-S-left-parens)))
    "C-)" (argless (if vemv/shortcuts/ruby/primary-right-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-right-parens vemv/shortcuts/ruby/primary-right-parens)))
    "C-M-)" (argless (if vemv/shortcuts/ruby/primary-secondary-right-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-right-parens vemv/shortcuts/ruby/primary-secondary-right-parens)))
    "C-S-)" (argless (if vemv/shortcuts/ruby/primary-S-right-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-right-parens vemv/shortcuts/ruby/primary-S-right-parens)))
    "C-]" (argless (if vemv/shortcuts/ruby/primary-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-right-bracket vemv/shortcuts/ruby/primary-right-bracket)))
    "C-M-]" (argless (if vemv/shortcuts/ruby/primary-secondary-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-right-bracket vemv/shortcuts/ruby/primary-secondary-right-bracket)))
    "C-S-]" (argless (if vemv/shortcuts/ruby/primary-S-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-right-bracket vemv/shortcuts/ruby/primary-S-right-bracket)))
    "C-{" (argless (if vemv/shortcuts/ruby/primary-left-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-left-curly vemv/shortcuts/ruby/primary-left-curly)))
    "C-M-{" (argless (if vemv/shortcuts/ruby/primary-secondary-left-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-left-curly vemv/shortcuts/ruby/primary-secondary-left-curly)))
    "C-S-{" (argless (if vemv/shortcuts/ruby/primary-S-left-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-left-curly vemv/shortcuts/ruby/primary-S-left-curly)))
    "C-}" (argless (if vemv/shortcuts/ruby/primary-right-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-right-curly vemv/shortcuts/ruby/primary-right-curly)))
    "C-M-}" (argless (if vemv/shortcuts/ruby/primary-secondary-right-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-right-curly vemv/shortcuts/ruby/primary-secondary-right-curly)))
    "C-S-}" (argless (if vemv/shortcuts/ruby/primary-S-right-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-right-curly vemv/shortcuts/ruby/primary-S-right-curly)))
    "C-*" (argless (if vemv/shortcuts/ruby/primary-star (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-star vemv/shortcuts/ruby/primary-star)))
    "C-M-*" (argless (if vemv/shortcuts/ruby/primary-secondary-star (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-star vemv/shortcuts/ruby/primary-secondary-star)))
    "C-S-*" (argless (if vemv/shortcuts/ruby/primary-S-star (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-star vemv/shortcuts/ruby/primary-S-star)))
    "C-/" (argless (if vemv/shortcuts/ruby/primary-slash (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-slash vemv/shortcuts/ruby/primary-slash)))
    "C-M-/" (argless (if vemv/shortcuts/ruby/primary-secondary-slash (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-slash vemv/shortcuts/ruby/primary-secondary-slash)))
    "C-S-/" (argless (if vemv/shortcuts/ruby/primary-S-slash (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-slash vemv/shortcuts/ruby/primary-S-slash)))
    "C-`" (argless (if vemv/shortcuts/ruby/primary-backtick (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-backtick vemv/shortcuts/ruby/primary-backtick)))
    "C-M-`" (argless (if vemv/shortcuts/ruby/primary-secondary-backtick (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-backtick vemv/shortcuts/ruby/primary-secondary-backtick)))
    "C-S-`" (argless (if vemv/shortcuts/ruby/primary-S-backtick (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-backtick vemv/shortcuts/ruby/primary-S-backtick)))
    "C-+" (argless (if vemv/shortcuts/ruby/primary-plus (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-plus vemv/shortcuts/ruby/primary-plus)))
    "C-M-+" (argless (if vemv/shortcuts/ruby/primary-secondary-plus (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-plus vemv/shortcuts/ruby/primary-secondary-plus)))
    "C-S-+" (argless (if vemv/shortcuts/ruby/primary-S-plus (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-plus vemv/shortcuts/ruby/primary-S-plus)))
    "C-<backspace>" (argless (if vemv/shortcuts/ruby/primary-backspace (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-backspace vemv/shortcuts/ruby/primary-backspace)))
    "C-M-<backspace>" (argless (if vemv/shortcuts/ruby/primary-secondary-backspace (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-backspace vemv/shortcuts/ruby/primary-secondary-backspace)))
    "C-S-<backspace>" (argless (if vemv/shortcuts/ruby/primary-S-backspace (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-backspace vemv/shortcuts/ruby/primary-S-backspace)))
    "C-<down>" (argless (if vemv/shortcuts/ruby/primary-down (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-down vemv/shortcuts/ruby/primary-down)))
    "C-M-<down>" (argless (if vemv/shortcuts/ruby/primary-secondary-down (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-down vemv/shortcuts/ruby/primary-secondary-down)))
    "C-S-<down>" (argless (if vemv/shortcuts/ruby/primary-S-down (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-down vemv/shortcuts/ruby/primary-S-down)))
    "C-<end>" (argless (if vemv/shortcuts/ruby/primary-end (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-end vemv/shortcuts/ruby/primary-end)))
    "C-M-<end>" (argless (if vemv/shortcuts/ruby/primary-secondary-end (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-end vemv/shortcuts/ruby/primary-secondary-end)))
    "C-S-<end>" (argless (if vemv/shortcuts/ruby/primary-S-end (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-end vemv/shortcuts/ruby/primary-S-end)))
    "C-<home>" (argless (if vemv/shortcuts/ruby/primary-home (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-home vemv/shortcuts/ruby/primary-home)))
    "C-M-<home>" (argless (if vemv/shortcuts/ruby/primary-secondary-home (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-home vemv/shortcuts/ruby/primary-secondary-home)))
    "C-S-<home>" (argless (if vemv/shortcuts/ruby/primary-S-home (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-home vemv/shortcuts/ruby/primary-S-home)))
    "C-<left>" (argless (if vemv/shortcuts/ruby/primary-left (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-left vemv/shortcuts/ruby/primary-left)))
    "C-M-<left>" (argless (if vemv/shortcuts/ruby/primary-secondary-left (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-left vemv/shortcuts/ruby/primary-secondary-left)))
    "C-S-<left>" (argless (if vemv/shortcuts/ruby/primary-S-left (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-left vemv/shortcuts/ruby/primary-S-left)))
    "C-<next>" (argless (if vemv/shortcuts/ruby/primary-next (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-next vemv/shortcuts/ruby/primary-next)))
    "C-M-<next>" (argless (if vemv/shortcuts/ruby/primary-secondary-next (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-next vemv/shortcuts/ruby/primary-secondary-next)))
    "C-S-<next>" (argless (if vemv/shortcuts/ruby/primary-S-next (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-next vemv/shortcuts/ruby/primary-S-next)))
    "C-<prior>" (argless (if vemv/shortcuts/ruby/primary-prior (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-prior vemv/shortcuts/ruby/primary-prior)))
    "C-M-<prior>" (argless (if vemv/shortcuts/ruby/primary-secondary-prior (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-prior vemv/shortcuts/ruby/primary-secondary-prior)))
    "C-S-<prior>" (argless (if vemv/shortcuts/ruby/primary-S-prior (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-prior vemv/shortcuts/ruby/primary-S-prior)))
    "C-<right>" (argless (if vemv/shortcuts/ruby/primary-right (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-right vemv/shortcuts/ruby/primary-right)))
    "C-M-<right>" (argless (if vemv/shortcuts/ruby/primary-secondary-right (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-right vemv/shortcuts/ruby/primary-secondary-right)))
    "C-S-<right>" (argless (if vemv/shortcuts/ruby/primary-S-right (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-right vemv/shortcuts/ruby/primary-S-right)))
    "C-<up>" (argless (if vemv/shortcuts/ruby/primary-up (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-up vemv/shortcuts/ruby/primary-up)))
    "C-M-<up>" (argless (if vemv/shortcuts/ruby/primary-secondary-up (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-up vemv/shortcuts/ruby/primary-secondary-up)))
    "C-S-<up>" (argless (if vemv/shortcuts/ruby/primary-S-up (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-up vemv/shortcuts/ruby/primary-S-up)))
    "C-=" (argless (if vemv/shortcuts/ruby/primary-equal (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-equal vemv/shortcuts/ruby/primary-equal)))
    "C-M-=" (argless (if vemv/shortcuts/ruby/primary-secondary-equal (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-equal vemv/shortcuts/ruby/primary-secondary-equal)))
    "C-S-=" (argless (if vemv/shortcuts/ruby/primary-S-equal (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-equal vemv/shortcuts/ruby/primary-S-equal)))
    "C-|" (argless (if vemv/shortcuts/ruby/primary-bar (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-bar vemv/shortcuts/ruby/primary-bar)))
    "C-M-|" (argless (if vemv/shortcuts/ruby/primary-secondary-bar (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-bar vemv/shortcuts/ruby/primary-secondary-bar)))
    "C-S-|" (argless (if vemv/shortcuts/ruby/primary-S-bar (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-bar vemv/shortcuts/ruby/primary-S-bar)))
    "C-RET" (argless (if vemv/shortcuts/ruby/primary-RET (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-RET vemv/shortcuts/ruby/primary-RET)))
    "C-M-RET" (argless (if vemv/shortcuts/ruby/primary-secondary-RET (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-RET vemv/shortcuts/ruby/primary-secondary-RET)))
    "C-S-RET" (argless (if vemv/shortcuts/ruby/primary-S-RET (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-RET vemv/shortcuts/ruby/primary-S-RET)))
    "C-SPC" (argless (if vemv/shortcuts/ruby/primary-SPC (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-SPC vemv/shortcuts/ruby/primary-SPC)))
    "C-M-SPC" (argless (if vemv/shortcuts/ruby/primary-secondary-SPC (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-secondary-SPC vemv/shortcuts/ruby/primary-secondary-SPC)))
    "C-S-SPC" (argless (if vemv/shortcuts/ruby/primary-S-SPC (vemv/keyboard-funcall :vemv/shortcuts/ruby/primary-S-SPC vemv/shortcuts/ruby/primary-S-SPC)))
    "M-a" (argless (if vemv/shortcuts/ruby/secondary-a (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-a vemv/shortcuts/ruby/secondary-a)))
    "M-S-a" (argless (if vemv/shortcuts/ruby/secondary-S-a (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-a vemv/shortcuts/ruby/secondary-S-a)))
    "M-b" (argless (if vemv/shortcuts/ruby/secondary-b (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-b vemv/shortcuts/ruby/secondary-b)))
    "M-S-b" (argless (if vemv/shortcuts/ruby/secondary-S-b (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-b vemv/shortcuts/ruby/secondary-S-b)))
    "M-c" (argless (if vemv/shortcuts/ruby/secondary-c (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-c vemv/shortcuts/ruby/secondary-c)))
    "M-S-c" (argless (if vemv/shortcuts/ruby/secondary-S-c (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-c vemv/shortcuts/ruby/secondary-S-c)))
    "M-d" (argless (if vemv/shortcuts/ruby/secondary-d (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-d vemv/shortcuts/ruby/secondary-d)))
    "M-S-d" (argless (if vemv/shortcuts/ruby/secondary-S-d (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-d vemv/shortcuts/ruby/secondary-S-d)))
    "M-e" (argless (if vemv/shortcuts/ruby/secondary-e (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-e vemv/shortcuts/ruby/secondary-e)))
    "M-S-e" (argless (if vemv/shortcuts/ruby/secondary-S-e (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-e vemv/shortcuts/ruby/secondary-S-e)))
    "M-f" (argless (if vemv/shortcuts/ruby/secondary-f (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-f vemv/shortcuts/ruby/secondary-f)))
    "M-S-f" (argless (if vemv/shortcuts/ruby/secondary-S-f (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-f vemv/shortcuts/ruby/secondary-S-f)))
    "M-g" (argless (if vemv/shortcuts/ruby/secondary-g (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-g vemv/shortcuts/ruby/secondary-g)))
    "M-S-g" (argless (if vemv/shortcuts/ruby/secondary-S-g (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-g vemv/shortcuts/ruby/secondary-S-g)))
    "M-h" (argless (if vemv/shortcuts/ruby/secondary-h (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-h vemv/shortcuts/ruby/secondary-h)))
    "M-S-h" (argless (if vemv/shortcuts/ruby/secondary-S-h (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-h vemv/shortcuts/ruby/secondary-S-h)))
    "M-i" (argless (if vemv/shortcuts/ruby/secondary-i (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-i vemv/shortcuts/ruby/secondary-i)))
    "M-S-i" (argless (if vemv/shortcuts/ruby/secondary-S-i (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-i vemv/shortcuts/ruby/secondary-S-i)))
    "M-j" (argless (if vemv/shortcuts/ruby/secondary-j (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-j vemv/shortcuts/ruby/secondary-j)))
    "M-S-j" (argless (if vemv/shortcuts/ruby/secondary-S-j (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-j vemv/shortcuts/ruby/secondary-S-j)))
    "M-k" (argless (if vemv/shortcuts/ruby/secondary-k (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-k vemv/shortcuts/ruby/secondary-k)))
    "M-S-k" (argless (if vemv/shortcuts/ruby/secondary-S-k (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-k vemv/shortcuts/ruby/secondary-S-k)))
    "M-l" (argless (if vemv/shortcuts/ruby/secondary-l (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-l vemv/shortcuts/ruby/secondary-l)))
    "M-S-l" (argless (if vemv/shortcuts/ruby/secondary-S-l (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-l vemv/shortcuts/ruby/secondary-S-l)))
    "M-m" (argless (if vemv/shortcuts/ruby/secondary-m (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-m vemv/shortcuts/ruby/secondary-m)))
    "M-S-m" (argless (if vemv/shortcuts/ruby/secondary-S-m (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-m vemv/shortcuts/ruby/secondary-S-m)))
    "M-n" (argless (if vemv/shortcuts/ruby/secondary-n (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-n vemv/shortcuts/ruby/secondary-n)))
    "M-S-n" (argless (if vemv/shortcuts/ruby/secondary-S-n (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-n vemv/shortcuts/ruby/secondary-S-n)))
    "M-o" (argless (if vemv/shortcuts/ruby/secondary-o (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-o vemv/shortcuts/ruby/secondary-o)))
    "M-S-o" (argless (if vemv/shortcuts/ruby/secondary-S-o (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-o vemv/shortcuts/ruby/secondary-S-o)))
    "M-p" (argless (if vemv/shortcuts/ruby/secondary-p (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-p vemv/shortcuts/ruby/secondary-p)))
    "M-S-p" (argless (if vemv/shortcuts/ruby/secondary-S-p (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-p vemv/shortcuts/ruby/secondary-S-p)))
    "M-q" (argless (if vemv/shortcuts/ruby/secondary-q (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-q vemv/shortcuts/ruby/secondary-q)))
    "M-S-q" (argless (if vemv/shortcuts/ruby/secondary-S-q (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-q vemv/shortcuts/ruby/secondary-S-q)))
    "M-r" (argless (if vemv/shortcuts/ruby/secondary-r (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-r vemv/shortcuts/ruby/secondary-r)))
    "M-S-r" (argless (if vemv/shortcuts/ruby/secondary-S-r (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-r vemv/shortcuts/ruby/secondary-S-r)))
    "M-s" (argless (if vemv/shortcuts/ruby/secondary-s (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-s vemv/shortcuts/ruby/secondary-s)))
    "M-S-s" (argless (if vemv/shortcuts/ruby/secondary-S-s (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-s vemv/shortcuts/ruby/secondary-S-s)))
    "M-t" (argless (if vemv/shortcuts/ruby/secondary-t (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-t vemv/shortcuts/ruby/secondary-t)))
    "M-S-t" (argless (if vemv/shortcuts/ruby/secondary-S-t (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-t vemv/shortcuts/ruby/secondary-S-t)))
    "M-u" (argless (if vemv/shortcuts/ruby/secondary-u (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-u vemv/shortcuts/ruby/secondary-u)))
    "M-S-u" (argless (if vemv/shortcuts/ruby/secondary-S-u (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-u vemv/shortcuts/ruby/secondary-S-u)))
    "M-v" (argless (if vemv/shortcuts/ruby/secondary-v (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-v vemv/shortcuts/ruby/secondary-v)))
    "M-S-v" (argless (if vemv/shortcuts/ruby/secondary-S-v (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-v vemv/shortcuts/ruby/secondary-S-v)))
    "M-w" (argless (if vemv/shortcuts/ruby/secondary-w (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-w vemv/shortcuts/ruby/secondary-w)))
    "M-S-w" (argless (if vemv/shortcuts/ruby/secondary-S-w (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-w vemv/shortcuts/ruby/secondary-S-w)))
    "M-x" (argless (if vemv/shortcuts/ruby/secondary-x (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-x vemv/shortcuts/ruby/secondary-x)))
    "M-S-x" (argless (if vemv/shortcuts/ruby/secondary-S-x (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-x vemv/shortcuts/ruby/secondary-S-x)))
    "M-y" (argless (if vemv/shortcuts/ruby/secondary-y (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-y vemv/shortcuts/ruby/secondary-y)))
    "M-S-y" (argless (if vemv/shortcuts/ruby/secondary-S-y (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-y vemv/shortcuts/ruby/secondary-S-y)))
    "M-z" (argless (if vemv/shortcuts/ruby/secondary-z (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-z vemv/shortcuts/ruby/secondary-z)))
    "M-S-z" (argless (if vemv/shortcuts/ruby/secondary-S-z (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-z vemv/shortcuts/ruby/secondary-S-z)))
    "M-0" (argless (if vemv/shortcuts/ruby/secondary-0 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-0 vemv/shortcuts/ruby/secondary-0)))
    "M-S-0" (argless (if vemv/shortcuts/ruby/secondary-S-0 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-0 vemv/shortcuts/ruby/secondary-S-0)))
    "M-1" (argless (if vemv/shortcuts/ruby/secondary-1 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-1 vemv/shortcuts/ruby/secondary-1)))
    "M-S-1" (argless (if vemv/shortcuts/ruby/secondary-S-1 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-1 vemv/shortcuts/ruby/secondary-S-1)))
    "M-2" (argless (if vemv/shortcuts/ruby/secondary-2 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-2 vemv/shortcuts/ruby/secondary-2)))
    "M-S-2" (argless (if vemv/shortcuts/ruby/secondary-S-2 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-2 vemv/shortcuts/ruby/secondary-S-2)))
    "M-3" (argless (if vemv/shortcuts/ruby/secondary-3 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-3 vemv/shortcuts/ruby/secondary-3)))
    "M-S-3" (argless (if vemv/shortcuts/ruby/secondary-S-3 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-3 vemv/shortcuts/ruby/secondary-S-3)))
    "M-4" (argless (if vemv/shortcuts/ruby/secondary-4 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-4 vemv/shortcuts/ruby/secondary-4)))
    "M-S-4" (argless (if vemv/shortcuts/ruby/secondary-S-4 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-4 vemv/shortcuts/ruby/secondary-S-4)))
    "M-5" (argless (if vemv/shortcuts/ruby/secondary-5 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-5 vemv/shortcuts/ruby/secondary-5)))
    "M-S-5" (argless (if vemv/shortcuts/ruby/secondary-S-5 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-5 vemv/shortcuts/ruby/secondary-S-5)))
    "M-6" (argless (if vemv/shortcuts/ruby/secondary-6 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-6 vemv/shortcuts/ruby/secondary-6)))
    "M-S-6" (argless (if vemv/shortcuts/ruby/secondary-S-6 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-6 vemv/shortcuts/ruby/secondary-S-6)))
    "M-7" (argless (if vemv/shortcuts/ruby/secondary-7 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-7 vemv/shortcuts/ruby/secondary-7)))
    "M-S-7" (argless (if vemv/shortcuts/ruby/secondary-S-7 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-7 vemv/shortcuts/ruby/secondary-S-7)))
    "M-8" (argless (if vemv/shortcuts/ruby/secondary-8 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-8 vemv/shortcuts/ruby/secondary-8)))
    "M-S-8" (argless (if vemv/shortcuts/ruby/secondary-S-8 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-8 vemv/shortcuts/ruby/secondary-S-8)))
    "M-9" (argless (if vemv/shortcuts/ruby/secondary-9 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-9 vemv/shortcuts/ruby/secondary-9)))
    "M-S-9" (argless (if vemv/shortcuts/ruby/secondary-S-9 (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-9 vemv/shortcuts/ruby/secondary-S-9)))
    "M-!" (argless (if vemv/shortcuts/ruby/secondary-bang (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-bang vemv/shortcuts/ruby/secondary-bang)))
    "M-S-!" (argless (if vemv/shortcuts/ruby/secondary-S-bang (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-bang vemv/shortcuts/ruby/secondary-S-bang)))
    "M-@" (argless (if vemv/shortcuts/ruby/secondary-at (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-at vemv/shortcuts/ruby/secondary-at)))
    "M-S-@" (argless (if vemv/shortcuts/ruby/secondary-S-at (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-at vemv/shortcuts/ruby/secondary-S-at)))
    "M-&" (argless (if vemv/shortcuts/ruby/secondary-ampersand (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-ampersand vemv/shortcuts/ruby/secondary-ampersand)))
    "M-S-&" (argless (if vemv/shortcuts/ruby/secondary-S-ampersand (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-ampersand vemv/shortcuts/ruby/secondary-S-ampersand)))
    "M-#" (argless (if vemv/shortcuts/ruby/secondary-hash (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-hash vemv/shortcuts/ruby/secondary-hash)))
    "M-S-#" (argless (if vemv/shortcuts/ruby/secondary-S-hash (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-hash vemv/shortcuts/ruby/secondary-S-hash)))
    "M-%" (argless (if vemv/shortcuts/ruby/secondary-percent (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-percent vemv/shortcuts/ruby/secondary-percent)))
    "M-S-%" (argless (if vemv/shortcuts/ruby/secondary-S-percent (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-percent vemv/shortcuts/ruby/secondary-S-percent)))
    "M-^" (argless (if vemv/shortcuts/ruby/secondary-caret (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-caret vemv/shortcuts/ruby/secondary-caret)))
    "M-S-^" (argless (if vemv/shortcuts/ruby/secondary-S-caret (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-caret vemv/shortcuts/ruby/secondary-S-caret)))
    "M-~" (argless (if vemv/shortcuts/ruby/secondary-tilde (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-tilde vemv/shortcuts/ruby/secondary-tilde)))
    "M-S-~" (argless (if vemv/shortcuts/ruby/secondary-S-tilde (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-tilde vemv/shortcuts/ruby/secondary-S-tilde)))
    "M-$" (argless (if vemv/shortcuts/ruby/secondary-dollar (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-dollar vemv/shortcuts/ruby/secondary-dollar)))
    "M-S-$" (argless (if vemv/shortcuts/ruby/secondary-S-dollar (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-dollar vemv/shortcuts/ruby/secondary-S-dollar)))
    "M-_" (argless (if vemv/shortcuts/ruby/secondary-underscore (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-underscore vemv/shortcuts/ruby/secondary-underscore)))
    "M-S-_" (argless (if vemv/shortcuts/ruby/secondary-S-underscore (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-underscore vemv/shortcuts/ruby/secondary-S-underscore)))
    "M--" (argless (if vemv/shortcuts/ruby/secondary-dash (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-dash vemv/shortcuts/ruby/secondary-dash)))
    "M-S--" (argless (if vemv/shortcuts/ruby/secondary-S-dash (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-dash vemv/shortcuts/ruby/secondary-S-dash)))
    "M-," (argless (if vemv/shortcuts/ruby/secondary-comma (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-comma vemv/shortcuts/ruby/secondary-comma)))
    "M-S-," (argless (if vemv/shortcuts/ruby/secondary-S-comma (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-comma vemv/shortcuts/ruby/secondary-S-comma)))
    "M-;" (argless (if vemv/shortcuts/ruby/secondary-semicolon (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-semicolon vemv/shortcuts/ruby/secondary-semicolon)))
    "M-S-;" (argless (if vemv/shortcuts/ruby/secondary-S-semicolon (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-semicolon vemv/shortcuts/ruby/secondary-S-semicolon)))
    "M-:" (argless (if vemv/shortcuts/ruby/secondary-colon (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-colon vemv/shortcuts/ruby/secondary-colon)))
    "M-S-:" (argless (if vemv/shortcuts/ruby/secondary-S-colon (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-colon vemv/shortcuts/ruby/secondary-S-colon)))
    "M-?" (argless (if vemv/shortcuts/ruby/secondary-question-mark (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-question-mark vemv/shortcuts/ruby/secondary-question-mark)))
    "M-S-?" (argless (if vemv/shortcuts/ruby/secondary-S-question-mark (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-question-mark vemv/shortcuts/ruby/secondary-S-question-mark)))
    "M-." (argless (if vemv/shortcuts/ruby/secondary-dot (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-dot vemv/shortcuts/ruby/secondary-dot)))
    "M-S-." (argless (if vemv/shortcuts/ruby/secondary-S-dot (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-dot vemv/shortcuts/ruby/secondary-S-dot)))
    "M-'" (argless (if vemv/shortcuts/ruby/secondary-single-quote (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-single-quote vemv/shortcuts/ruby/secondary-single-quote)))
    "M-S-'" (argless (if vemv/shortcuts/ruby/secondary-S-single-quote (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-single-quote vemv/shortcuts/ruby/secondary-S-single-quote)))
    "M-(" (argless (if vemv/shortcuts/ruby/secondary-left-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-left-parens vemv/shortcuts/ruby/secondary-left-parens)))
    "M-S-(" (argless (if vemv/shortcuts/ruby/secondary-S-left-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-left-parens vemv/shortcuts/ruby/secondary-S-left-parens)))
    "M-)" (argless (if vemv/shortcuts/ruby/secondary-right-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-right-parens vemv/shortcuts/ruby/secondary-right-parens)))
    "M-S-)" (argless (if vemv/shortcuts/ruby/secondary-S-right-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-right-parens vemv/shortcuts/ruby/secondary-S-right-parens)))
    "M-[" (argless (if vemv/shortcuts/ruby/secondary-left-bracket (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-left-bracket vemv/shortcuts/ruby/secondary-left-bracket)))
    "M-S-[" (argless (if vemv/shortcuts/ruby/secondary-S-left-bracket (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-left-bracket vemv/shortcuts/ruby/secondary-S-left-bracket)))
    "M-]" (argless (if vemv/shortcuts/ruby/secondary-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-right-bracket vemv/shortcuts/ruby/secondary-right-bracket)))
    "M-S-]" (argless (if vemv/shortcuts/ruby/secondary-S-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-right-bracket vemv/shortcuts/ruby/secondary-S-right-bracket)))
    "M-{" (argless (if vemv/shortcuts/ruby/secondary-left-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-left-curly vemv/shortcuts/ruby/secondary-left-curly)))
    "M-S-{" (argless (if vemv/shortcuts/ruby/secondary-S-left-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-left-curly vemv/shortcuts/ruby/secondary-S-left-curly)))
    "M-}" (argless (if vemv/shortcuts/ruby/secondary-right-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-right-curly vemv/shortcuts/ruby/secondary-right-curly)))
    "M-S-}" (argless (if vemv/shortcuts/ruby/secondary-S-right-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-right-curly vemv/shortcuts/ruby/secondary-S-right-curly)))
    "M-*" (argless (if vemv/shortcuts/ruby/secondary-star (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-star vemv/shortcuts/ruby/secondary-star)))
    "M-S-*" (argless (if vemv/shortcuts/ruby/secondary-S-star (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-star vemv/shortcuts/ruby/secondary-S-star)))
    "M-/" (argless (if vemv/shortcuts/ruby/secondary-slash (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-slash vemv/shortcuts/ruby/secondary-slash)))
    "M-S-/" (argless (if vemv/shortcuts/ruby/secondary-S-slash (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-slash vemv/shortcuts/ruby/secondary-S-slash)))
    "M-`" (argless (if vemv/shortcuts/ruby/secondary-backtick (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-backtick vemv/shortcuts/ruby/secondary-backtick)))
    "M-S-`" (argless (if vemv/shortcuts/ruby/secondary-S-backtick (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-backtick vemv/shortcuts/ruby/secondary-S-backtick)))
    "M-+" (argless (if vemv/shortcuts/ruby/secondary-plus (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-plus vemv/shortcuts/ruby/secondary-plus)))
    "M-S-+" (argless (if vemv/shortcuts/ruby/secondary-S-plus (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-plus vemv/shortcuts/ruby/secondary-S-plus)))
    "M-<backspace>" (argless (if vemv/shortcuts/ruby/secondary-backspace (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-backspace vemv/shortcuts/ruby/secondary-backspace)))
    "M-S-<backspace>" (argless (if vemv/shortcuts/ruby/secondary-S-backspace (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-backspace vemv/shortcuts/ruby/secondary-S-backspace)))
    "M-<down>" (argless (if vemv/shortcuts/ruby/secondary-down (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-down vemv/shortcuts/ruby/secondary-down)))
    "M-S-<down>" (argless (if vemv/shortcuts/ruby/secondary-S-down (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-down vemv/shortcuts/ruby/secondary-S-down)))
    "M-<end>" (argless (if vemv/shortcuts/ruby/secondary-end (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-end vemv/shortcuts/ruby/secondary-end)))
    "M-S-<end>" (argless (if vemv/shortcuts/ruby/secondary-S-end (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-end vemv/shortcuts/ruby/secondary-S-end)))
    "M-<home>" (argless (if vemv/shortcuts/ruby/secondary-home (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-home vemv/shortcuts/ruby/secondary-home)))
    "M-S-<home>" (argless (if vemv/shortcuts/ruby/secondary-S-home (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-home vemv/shortcuts/ruby/secondary-S-home)))
    "M-<left>" (argless (if vemv/shortcuts/ruby/secondary-left (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-left vemv/shortcuts/ruby/secondary-left)))
    "M-S-<left>" (argless (if vemv/shortcuts/ruby/secondary-S-left (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-left vemv/shortcuts/ruby/secondary-S-left)))
    "M-<next>" (argless (if vemv/shortcuts/ruby/secondary-next (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-next vemv/shortcuts/ruby/secondary-next)))
    "M-S-<next>" (argless (if vemv/shortcuts/ruby/secondary-S-next (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-next vemv/shortcuts/ruby/secondary-S-next)))
    "M-<prior>" (argless (if vemv/shortcuts/ruby/secondary-prior (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-prior vemv/shortcuts/ruby/secondary-prior)))
    "M-S-<prior>" (argless (if vemv/shortcuts/ruby/secondary-S-prior (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-prior vemv/shortcuts/ruby/secondary-S-prior)))
    "M-<right>" (argless (if vemv/shortcuts/ruby/secondary-right (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-right vemv/shortcuts/ruby/secondary-right)))
    "M-S-<right>" (argless (if vemv/shortcuts/ruby/secondary-S-right (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-right vemv/shortcuts/ruby/secondary-S-right)))
    "M-<up>" (argless (if vemv/shortcuts/ruby/secondary-up (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-up vemv/shortcuts/ruby/secondary-up)))
    "M-S-<up>" (argless (if vemv/shortcuts/ruby/secondary-S-up (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-up vemv/shortcuts/ruby/secondary-S-up)))
    "M-=" (argless (if vemv/shortcuts/ruby/secondary-equal (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-equal vemv/shortcuts/ruby/secondary-equal)))
    "M-S-=" (argless (if vemv/shortcuts/ruby/secondary-S-equal (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-equal vemv/shortcuts/ruby/secondary-S-equal)))
    "M-|" (argless (if vemv/shortcuts/ruby/secondary-bar (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-bar vemv/shortcuts/ruby/secondary-bar)))
    "M-S-|" (argless (if vemv/shortcuts/ruby/secondary-S-bar (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-bar vemv/shortcuts/ruby/secondary-S-bar)))
    "M-RET" (argless (if vemv/shortcuts/ruby/secondary-RET (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-RET vemv/shortcuts/ruby/secondary-RET)))
    "M-S-RET" (argless (if vemv/shortcuts/ruby/secondary-S-RET (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-RET vemv/shortcuts/ruby/secondary-S-RET)))
    "M-SPC" (argless (if vemv/shortcuts/ruby/secondary-SPC (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-SPC vemv/shortcuts/ruby/secondary-SPC)))
    "M-S-SPC" (argless (if vemv/shortcuts/ruby/secondary-S-SPC (vemv/keyboard-funcall :vemv/shortcuts/ruby/secondary-S-SPC vemv/shortcuts/ruby/secondary-S-SPC)))
    "s-a" (argless (if vemv/shortcuts/ruby/tertiary-a (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-a vemv/shortcuts/ruby/tertiary-a)))
    "s-S-a" (argless (if vemv/shortcuts/ruby/tertiary-S-a (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-a vemv/shortcuts/ruby/tertiary-S-a)))
    "s-b" (argless (if vemv/shortcuts/ruby/tertiary-b (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-b vemv/shortcuts/ruby/tertiary-b)))
    "s-S-b" (argless (if vemv/shortcuts/ruby/tertiary-S-b (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-b vemv/shortcuts/ruby/tertiary-S-b)))
    "s-c" (argless (if vemv/shortcuts/ruby/tertiary-c (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-c vemv/shortcuts/ruby/tertiary-c)))
    "s-S-c" (argless (if vemv/shortcuts/ruby/tertiary-S-c (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-c vemv/shortcuts/ruby/tertiary-S-c)))
    "s-d" (argless (if vemv/shortcuts/ruby/tertiary-d (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-d vemv/shortcuts/ruby/tertiary-d)))
    "s-S-d" (argless (if vemv/shortcuts/ruby/tertiary-S-d (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-d vemv/shortcuts/ruby/tertiary-S-d)))
    "s-e" (argless (if vemv/shortcuts/ruby/tertiary-e (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-e vemv/shortcuts/ruby/tertiary-e)))
    "s-S-e" (argless (if vemv/shortcuts/ruby/tertiary-S-e (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-e vemv/shortcuts/ruby/tertiary-S-e)))
    "s-f" (argless (if vemv/shortcuts/ruby/tertiary-f (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-f vemv/shortcuts/ruby/tertiary-f)))
    "s-S-f" (argless (if vemv/shortcuts/ruby/tertiary-S-f (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-f vemv/shortcuts/ruby/tertiary-S-f)))
    "s-g" (argless (if vemv/shortcuts/ruby/tertiary-g (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-g vemv/shortcuts/ruby/tertiary-g)))
    "s-S-g" (argless (if vemv/shortcuts/ruby/tertiary-S-g (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-g vemv/shortcuts/ruby/tertiary-S-g)))
    "s-h" (argless (if vemv/shortcuts/ruby/tertiary-h (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-h vemv/shortcuts/ruby/tertiary-h)))
    "s-S-h" (argless (if vemv/shortcuts/ruby/tertiary-S-h (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-h vemv/shortcuts/ruby/tertiary-S-h)))
    "s-i" (argless (if vemv/shortcuts/ruby/tertiary-i (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-i vemv/shortcuts/ruby/tertiary-i)))
    "s-S-i" (argless (if vemv/shortcuts/ruby/tertiary-S-i (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-i vemv/shortcuts/ruby/tertiary-S-i)))
    "s-j" (argless (if vemv/shortcuts/ruby/tertiary-j (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-j vemv/shortcuts/ruby/tertiary-j)))
    "s-S-j" (argless (if vemv/shortcuts/ruby/tertiary-S-j (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-j vemv/shortcuts/ruby/tertiary-S-j)))
    "s-k" (argless (if vemv/shortcuts/ruby/tertiary-k (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-k vemv/shortcuts/ruby/tertiary-k)))
    "s-S-k" (argless (if vemv/shortcuts/ruby/tertiary-S-k (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-k vemv/shortcuts/ruby/tertiary-S-k)))
    "s-l" (argless (if vemv/shortcuts/ruby/tertiary-l (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-l vemv/shortcuts/ruby/tertiary-l)))
    "s-S-l" (argless (if vemv/shortcuts/ruby/tertiary-S-l (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-l vemv/shortcuts/ruby/tertiary-S-l)))
    "s-m" (argless (if vemv/shortcuts/ruby/tertiary-m (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-m vemv/shortcuts/ruby/tertiary-m)))
    "s-S-m" (argless (if vemv/shortcuts/ruby/tertiary-S-m (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-m vemv/shortcuts/ruby/tertiary-S-m)))
    "s-n" (argless (if vemv/shortcuts/ruby/tertiary-n (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-n vemv/shortcuts/ruby/tertiary-n)))
    "s-S-n" (argless (if vemv/shortcuts/ruby/tertiary-S-n (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-n vemv/shortcuts/ruby/tertiary-S-n)))
    "s-o" (argless (if vemv/shortcuts/ruby/tertiary-o (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-o vemv/shortcuts/ruby/tertiary-o)))
    "s-S-o" (argless (if vemv/shortcuts/ruby/tertiary-S-o (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-o vemv/shortcuts/ruby/tertiary-S-o)))
    "s-p" (argless (if vemv/shortcuts/ruby/tertiary-p (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-p vemv/shortcuts/ruby/tertiary-p)))
    "s-S-p" (argless (if vemv/shortcuts/ruby/tertiary-S-p (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-p vemv/shortcuts/ruby/tertiary-S-p)))
    "s-q" (argless (if vemv/shortcuts/ruby/tertiary-q (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-q vemv/shortcuts/ruby/tertiary-q)))
    "s-S-q" (argless (if vemv/shortcuts/ruby/tertiary-S-q (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-q vemv/shortcuts/ruby/tertiary-S-q)))
    "s-r" (argless (if vemv/shortcuts/ruby/tertiary-r (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-r vemv/shortcuts/ruby/tertiary-r)))
    "s-S-r" (argless (if vemv/shortcuts/ruby/tertiary-S-r (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-r vemv/shortcuts/ruby/tertiary-S-r)))
    "s-s" (argless (if vemv/shortcuts/ruby/tertiary-s (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-s vemv/shortcuts/ruby/tertiary-s)))
    "s-S-s" (argless (if vemv/shortcuts/ruby/tertiary-S-s (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-s vemv/shortcuts/ruby/tertiary-S-s)))
    "s-t" (argless (if vemv/shortcuts/ruby/tertiary-t (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-t vemv/shortcuts/ruby/tertiary-t)))
    "s-S-t" (argless (if vemv/shortcuts/ruby/tertiary-S-t (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-t vemv/shortcuts/ruby/tertiary-S-t)))
    "s-u" (argless (if vemv/shortcuts/ruby/tertiary-u (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-u vemv/shortcuts/ruby/tertiary-u)))
    "s-S-u" (argless (if vemv/shortcuts/ruby/tertiary-S-u (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-u vemv/shortcuts/ruby/tertiary-S-u)))
    "s-v" (argless (if vemv/shortcuts/ruby/tertiary-v (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-v vemv/shortcuts/ruby/tertiary-v)))
    "s-S-v" (argless (if vemv/shortcuts/ruby/tertiary-S-v (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-v vemv/shortcuts/ruby/tertiary-S-v)))
    "s-w" (argless (if vemv/shortcuts/ruby/tertiary-w (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-w vemv/shortcuts/ruby/tertiary-w)))
    "s-S-w" (argless (if vemv/shortcuts/ruby/tertiary-S-w (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-w vemv/shortcuts/ruby/tertiary-S-w)))
    "s-x" (argless (if vemv/shortcuts/ruby/tertiary-x (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-x vemv/shortcuts/ruby/tertiary-x)))
    "s-S-x" (argless (if vemv/shortcuts/ruby/tertiary-S-x (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-x vemv/shortcuts/ruby/tertiary-S-x)))
    "s-y" (argless (if vemv/shortcuts/ruby/tertiary-y (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-y vemv/shortcuts/ruby/tertiary-y)))
    "s-S-y" (argless (if vemv/shortcuts/ruby/tertiary-S-y (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-y vemv/shortcuts/ruby/tertiary-S-y)))
    "s-z" (argless (if vemv/shortcuts/ruby/tertiary-z (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-z vemv/shortcuts/ruby/tertiary-z)))
    "s-S-z" (argless (if vemv/shortcuts/ruby/tertiary-S-z (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-z vemv/shortcuts/ruby/tertiary-S-z)))
    "s-0" (argless (if vemv/shortcuts/ruby/tertiary-0 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-0 vemv/shortcuts/ruby/tertiary-0)))
    "s-S-0" (argless (if vemv/shortcuts/ruby/tertiary-S-0 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-0 vemv/shortcuts/ruby/tertiary-S-0)))
    "s-1" (argless (if vemv/shortcuts/ruby/tertiary-1 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-1 vemv/shortcuts/ruby/tertiary-1)))
    "s-S-1" (argless (if vemv/shortcuts/ruby/tertiary-S-1 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-1 vemv/shortcuts/ruby/tertiary-S-1)))
    "s-2" (argless (if vemv/shortcuts/ruby/tertiary-2 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-2 vemv/shortcuts/ruby/tertiary-2)))
    "s-S-2" (argless (if vemv/shortcuts/ruby/tertiary-S-2 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-2 vemv/shortcuts/ruby/tertiary-S-2)))
    "s-3" (argless (if vemv/shortcuts/ruby/tertiary-3 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-3 vemv/shortcuts/ruby/tertiary-3)))
    "s-S-3" (argless (if vemv/shortcuts/ruby/tertiary-S-3 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-3 vemv/shortcuts/ruby/tertiary-S-3)))
    "s-4" (argless (if vemv/shortcuts/ruby/tertiary-4 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-4 vemv/shortcuts/ruby/tertiary-4)))
    "s-S-4" (argless (if vemv/shortcuts/ruby/tertiary-S-4 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-4 vemv/shortcuts/ruby/tertiary-S-4)))
    "s-5" (argless (if vemv/shortcuts/ruby/tertiary-5 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-5 vemv/shortcuts/ruby/tertiary-5)))
    "s-S-5" (argless (if vemv/shortcuts/ruby/tertiary-S-5 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-5 vemv/shortcuts/ruby/tertiary-S-5)))
    "s-6" (argless (if vemv/shortcuts/ruby/tertiary-6 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-6 vemv/shortcuts/ruby/tertiary-6)))
    "s-S-6" (argless (if vemv/shortcuts/ruby/tertiary-S-6 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-6 vemv/shortcuts/ruby/tertiary-S-6)))
    "s-7" (argless (if vemv/shortcuts/ruby/tertiary-7 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-7 vemv/shortcuts/ruby/tertiary-7)))
    "s-S-7" (argless (if vemv/shortcuts/ruby/tertiary-S-7 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-7 vemv/shortcuts/ruby/tertiary-S-7)))
    "s-8" (argless (if vemv/shortcuts/ruby/tertiary-8 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-8 vemv/shortcuts/ruby/tertiary-8)))
    "s-S-8" (argless (if vemv/shortcuts/ruby/tertiary-S-8 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-8 vemv/shortcuts/ruby/tertiary-S-8)))
    "s-9" (argless (if vemv/shortcuts/ruby/tertiary-9 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-9 vemv/shortcuts/ruby/tertiary-9)))
    "s-S-9" (argless (if vemv/shortcuts/ruby/tertiary-S-9 (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-9 vemv/shortcuts/ruby/tertiary-S-9)))
    "s-!" (argless (if vemv/shortcuts/ruby/tertiary-bang (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-bang vemv/shortcuts/ruby/tertiary-bang)))
    "s-S-!" (argless (if vemv/shortcuts/ruby/tertiary-S-bang (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-bang vemv/shortcuts/ruby/tertiary-S-bang)))
    "s-@" (argless (if vemv/shortcuts/ruby/tertiary-at (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-at vemv/shortcuts/ruby/tertiary-at)))
    "s-S-@" (argless (if vemv/shortcuts/ruby/tertiary-S-at (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-at vemv/shortcuts/ruby/tertiary-S-at)))
    "s-&" (argless (if vemv/shortcuts/ruby/tertiary-ampersand (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-ampersand vemv/shortcuts/ruby/tertiary-ampersand)))
    "s-S-&" (argless (if vemv/shortcuts/ruby/tertiary-S-ampersand (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-ampersand vemv/shortcuts/ruby/tertiary-S-ampersand)))
    "s-#" (argless (if vemv/shortcuts/ruby/tertiary-hash (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-hash vemv/shortcuts/ruby/tertiary-hash)))
    "s-S-#" (argless (if vemv/shortcuts/ruby/tertiary-S-hash (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-hash vemv/shortcuts/ruby/tertiary-S-hash)))
    "s-%" (argless (if vemv/shortcuts/ruby/tertiary-percent (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-percent vemv/shortcuts/ruby/tertiary-percent)))
    "s-S-%" (argless (if vemv/shortcuts/ruby/tertiary-S-percent (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-percent vemv/shortcuts/ruby/tertiary-S-percent)))
    "s-^" (argless (if vemv/shortcuts/ruby/tertiary-caret (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-caret vemv/shortcuts/ruby/tertiary-caret)))
    "s-S-^" (argless (if vemv/shortcuts/ruby/tertiary-S-caret (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-caret vemv/shortcuts/ruby/tertiary-S-caret)))
    "s-~" (argless (if vemv/shortcuts/ruby/tertiary-tilde (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-tilde vemv/shortcuts/ruby/tertiary-tilde)))
    "s-S-~" (argless (if vemv/shortcuts/ruby/tertiary-S-tilde (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-tilde vemv/shortcuts/ruby/tertiary-S-tilde)))
    "s-$" (argless (if vemv/shortcuts/ruby/tertiary-dollar (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-dollar vemv/shortcuts/ruby/tertiary-dollar)))
    "s-S-$" (argless (if vemv/shortcuts/ruby/tertiary-S-dollar (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-dollar vemv/shortcuts/ruby/tertiary-S-dollar)))
    "s-_" (argless (if vemv/shortcuts/ruby/tertiary-underscore (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-underscore vemv/shortcuts/ruby/tertiary-underscore)))
    "s-S-_" (argless (if vemv/shortcuts/ruby/tertiary-S-underscore (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-underscore vemv/shortcuts/ruby/tertiary-S-underscore)))
    "s--" (argless (if vemv/shortcuts/ruby/tertiary-dash (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-dash vemv/shortcuts/ruby/tertiary-dash)))
    "s-S--" (argless (if vemv/shortcuts/ruby/tertiary-S-dash (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-dash vemv/shortcuts/ruby/tertiary-S-dash)))
    "s-," (argless (if vemv/shortcuts/ruby/tertiary-comma (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-comma vemv/shortcuts/ruby/tertiary-comma)))
    "s-S-," (argless (if vemv/shortcuts/ruby/tertiary-S-comma (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-comma vemv/shortcuts/ruby/tertiary-S-comma)))
    "s-;" (argless (if vemv/shortcuts/ruby/tertiary-semicolon (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-semicolon vemv/shortcuts/ruby/tertiary-semicolon)))
    "s-S-;" (argless (if vemv/shortcuts/ruby/tertiary-S-semicolon (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-semicolon vemv/shortcuts/ruby/tertiary-S-semicolon)))
    "s-:" (argless (if vemv/shortcuts/ruby/tertiary-colon (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-colon vemv/shortcuts/ruby/tertiary-colon)))
    "s-S-:" (argless (if vemv/shortcuts/ruby/tertiary-S-colon (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-colon vemv/shortcuts/ruby/tertiary-S-colon)))
    "s-?" (argless (if vemv/shortcuts/ruby/tertiary-question-mark (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-question-mark vemv/shortcuts/ruby/tertiary-question-mark)))
    "s-S-?" (argless (if vemv/shortcuts/ruby/tertiary-S-question-mark (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-question-mark vemv/shortcuts/ruby/tertiary-S-question-mark)))
    "s-." (argless (if vemv/shortcuts/ruby/tertiary-dot (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-dot vemv/shortcuts/ruby/tertiary-dot)))
    "s-S-." (argless (if vemv/shortcuts/ruby/tertiary-S-dot (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-dot vemv/shortcuts/ruby/tertiary-S-dot)))
    "s-'" (argless (if vemv/shortcuts/ruby/tertiary-single-quote (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-single-quote vemv/shortcuts/ruby/tertiary-single-quote)))
    "s-S-'" (argless (if vemv/shortcuts/ruby/tertiary-S-single-quote (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-single-quote vemv/shortcuts/ruby/tertiary-S-single-quote)))
    "s-(" (argless (if vemv/shortcuts/ruby/tertiary-left-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-left-parens vemv/shortcuts/ruby/tertiary-left-parens)))
    "s-S-(" (argless (if vemv/shortcuts/ruby/tertiary-S-left-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-left-parens vemv/shortcuts/ruby/tertiary-S-left-parens)))
    "s-)" (argless (if vemv/shortcuts/ruby/tertiary-right-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-right-parens vemv/shortcuts/ruby/tertiary-right-parens)))
    "s-S-)" (argless (if vemv/shortcuts/ruby/tertiary-S-right-parens (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-right-parens vemv/shortcuts/ruby/tertiary-S-right-parens)))
    "s-[" (argless (if vemv/shortcuts/ruby/tertiary-left-bracket (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-left-bracket vemv/shortcuts/ruby/tertiary-left-bracket)))
    "s-S-[" (argless (if vemv/shortcuts/ruby/tertiary-S-left-bracket (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-left-bracket vemv/shortcuts/ruby/tertiary-S-left-bracket)))
    "s-]" (argless (if vemv/shortcuts/ruby/tertiary-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-right-bracket vemv/shortcuts/ruby/tertiary-right-bracket)))
    "s-S-]" (argless (if vemv/shortcuts/ruby/tertiary-S-right-bracket (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-right-bracket vemv/shortcuts/ruby/tertiary-S-right-bracket)))
    "s-{" (argless (if vemv/shortcuts/ruby/tertiary-left-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-left-curly vemv/shortcuts/ruby/tertiary-left-curly)))
    "s-S-{" (argless (if vemv/shortcuts/ruby/tertiary-S-left-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-left-curly vemv/shortcuts/ruby/tertiary-S-left-curly)))
    "s-}" (argless (if vemv/shortcuts/ruby/tertiary-right-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-right-curly vemv/shortcuts/ruby/tertiary-right-curly)))
    "s-S-}" (argless (if vemv/shortcuts/ruby/tertiary-S-right-curly (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-right-curly vemv/shortcuts/ruby/tertiary-S-right-curly)))
    "s-*" (argless (if vemv/shortcuts/ruby/tertiary-star (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-star vemv/shortcuts/ruby/tertiary-star)))
    "s-S-*" (argless (if vemv/shortcuts/ruby/tertiary-S-star (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-star vemv/shortcuts/ruby/tertiary-S-star)))
    "s-/" (argless (if vemv/shortcuts/ruby/tertiary-slash (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-slash vemv/shortcuts/ruby/tertiary-slash)))
    "s-S-/" (argless (if vemv/shortcuts/ruby/tertiary-S-slash (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-slash vemv/shortcuts/ruby/tertiary-S-slash)))
    "s-`" (argless (if vemv/shortcuts/ruby/tertiary-backtick (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-backtick vemv/shortcuts/ruby/tertiary-backtick)))
    "s-S-`" (argless (if vemv/shortcuts/ruby/tertiary-S-backtick (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-backtick vemv/shortcuts/ruby/tertiary-S-backtick)))
    "s-+" (argless (if vemv/shortcuts/ruby/tertiary-plus (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-plus vemv/shortcuts/ruby/tertiary-plus)))
    "s-S-+" (argless (if vemv/shortcuts/ruby/tertiary-S-plus (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-plus vemv/shortcuts/ruby/tertiary-S-plus)))
    "s-<backspace>" (argless (if vemv/shortcuts/ruby/tertiary-backspace (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-backspace vemv/shortcuts/ruby/tertiary-backspace)))
    "s-S-<backspace>" (argless (if vemv/shortcuts/ruby/tertiary-S-backspace (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-backspace vemv/shortcuts/ruby/tertiary-S-backspace)))
    "s-<down>" (argless (if vemv/shortcuts/ruby/tertiary-down (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-down vemv/shortcuts/ruby/tertiary-down)))
    "s-S-<down>" (argless (if vemv/shortcuts/ruby/tertiary-S-down (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-down vemv/shortcuts/ruby/tertiary-S-down)))
    "s-<end>" (argless (if vemv/shortcuts/ruby/tertiary-end (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-end vemv/shortcuts/ruby/tertiary-end)))
    "s-S-<end>" (argless (if vemv/shortcuts/ruby/tertiary-S-end (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-end vemv/shortcuts/ruby/tertiary-S-end)))
    "s-<home>" (argless (if vemv/shortcuts/ruby/tertiary-home (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-home vemv/shortcuts/ruby/tertiary-home)))
    "s-S-<home>" (argless (if vemv/shortcuts/ruby/tertiary-S-home (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-home vemv/shortcuts/ruby/tertiary-S-home)))
    "s-<left>" (argless (if vemv/shortcuts/ruby/tertiary-left (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-left vemv/shortcuts/ruby/tertiary-left)))
    "s-S-<left>" (argless (if vemv/shortcuts/ruby/tertiary-S-left (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-left vemv/shortcuts/ruby/tertiary-S-left)))
    "s-<next>" (argless (if vemv/shortcuts/ruby/tertiary-next (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-next vemv/shortcuts/ruby/tertiary-next)))
    "s-S-<next>" (argless (if vemv/shortcuts/ruby/tertiary-S-next (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-next vemv/shortcuts/ruby/tertiary-S-next)))
    "s-<prior>" (argless (if vemv/shortcuts/ruby/tertiary-prior (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-prior vemv/shortcuts/ruby/tertiary-prior)))
    "s-S-<prior>" (argless (if vemv/shortcuts/ruby/tertiary-S-prior (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-prior vemv/shortcuts/ruby/tertiary-S-prior)))
    "s-<right>" (argless (if vemv/shortcuts/ruby/tertiary-right (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-right vemv/shortcuts/ruby/tertiary-right)))
    "s-S-<right>" (argless (if vemv/shortcuts/ruby/tertiary-S-right (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-right vemv/shortcuts/ruby/tertiary-S-right)))
    "s-<up>" (argless (if vemv/shortcuts/ruby/tertiary-up (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-up vemv/shortcuts/ruby/tertiary-up)))
    "s-S-<up>" (argless (if vemv/shortcuts/ruby/tertiary-S-up (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-up vemv/shortcuts/ruby/tertiary-S-up)))
    "s-=" (argless (if vemv/shortcuts/ruby/tertiary-equal (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-equal vemv/shortcuts/ruby/tertiary-equal)))
    "s-S-=" (argless (if vemv/shortcuts/ruby/tertiary-S-equal (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-equal vemv/shortcuts/ruby/tertiary-S-equal)))
    "s-|" (argless (if vemv/shortcuts/ruby/tertiary-bar (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-bar vemv/shortcuts/ruby/tertiary-bar)))
    "s-S-|" (argless (if vemv/shortcuts/ruby/tertiary-S-bar (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-bar vemv/shortcuts/ruby/tertiary-S-bar)))
    "s-RET" (argless (if vemv/shortcuts/ruby/tertiary-RET (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-RET vemv/shortcuts/ruby/tertiary-RET)))
    "s-S-RET" (argless (if vemv/shortcuts/ruby/tertiary-S-RET (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-RET vemv/shortcuts/ruby/tertiary-S-RET)))
    "s-SPC" (argless (if vemv/shortcuts/ruby/tertiary-SPC (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-SPC vemv/shortcuts/ruby/tertiary-SPC)))
    "s-S-SPC" (argless (if vemv/shortcuts/ruby/tertiary-S-SPC (vemv/keyboard-funcall :vemv/shortcuts/ruby/tertiary-S-SPC vemv/shortcuts/ruby/tertiary-S-SPC)))
))