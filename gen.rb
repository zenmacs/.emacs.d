# Certain C-<char> keybindings are forbidden
NO_C = %w([ i m g ~)

# there's no C-S-semicolon, but C-colon, and so on
DUALS = %w(` ~ + = [ ] { } | \\ ; : ' " , < . > / ? ! @ # $ % ^ & * ( ) - _ 1 2 3 4 5 6 7 8 9 0)

SELF_INSERTING = (DUALS + %w(SPC))

# XXX add all possible members of DUALS here
# no backlash for now - don't want to escape things
REPLACEMENTS = Hash.new{|map, key| key }.merge({
  '!' => 'bang',
  '@' => 'at',
  '&' => 'ampersand',
  '#' => 'hash',
  '%' => 'percent',
  '^' => 'caret',
  '~' => 'tilde',
  '$' => 'dollar',
  "_" => 'underscore',
  "-" => 'dash',
  "," => 'comma',
  ";" => 'semicolon',
  ":" => 'colon',
  "?" => 'question-mark',
  "." => 'dot',
  "'" => 'single-quote',
  "(" => 'left-parens',
  ")" => 'right-parens',
  "[" => 'left-bracket',
  "[f1]" => "f1",
  "[f10]" => "f10",
  "[f11]" => "f11",
  "[f12]" => "f12",
  "[f2]" => "f2",
  "[f3]" => "f3",
  "[f4]" => "f4",
  "[f5]" => "f5",
  "[f6]" => "f6",
  "[f7]" => "f7",
  "[f8]" => "f8",
  "[f9]" => "f9",
  "]" => 'right-bracket',
  "{" => 'left-curly',
  "}" => 'right-curly',
  "*" => 'star',
  "/" => 'slash',
  "`" => 'backtick',
  "+" => 'plus',
  "<backspace>" => "backspace",
  "<down>" => "down",
  "<end>" => "end",
  "<home>" => "home",
  "<left>" => "left",
  "<next>" => "next",
  "<prior>" => "prior",
  "<right>" => "right",
  "<up>" => "up",
  "=" => 'equal',
  "|" => 'bar',
  "RET" => "RET", # return key
  "SPC" => "SPC" # space key
})

SPECIAL = REPLACEMENTS.keys

def informative_stub binding
  %|(argless (message "You pressed `#{binding}`! For making this binding useful, customize it (you can find instructions at the wiki)"))|
end

def super_combination_for char
  char[/\[f\d+\]/] ? "<s-f#{char[/\d+/]}>" : "S-#{char}"
end

def emit_setqs scope: 'global', modifier_mappings: {"primary" => 'C', "secondary" => 'M', "tertiary" => 's'}

  result = "(provide 'vemv.shortcuts.#{scope}.base)\n;; generated with gen.rb\n"

  SPECIAL.each do |char|

    binding = "vemv/shortcuts/#{scope}/#{REPLACEMENTS[char]}"
    value = scope == 'global' ? informative_stub(binding) : "vemv/shortcuts/global/#{REPLACEMENTS[char]}"
    unless SELF_INSERTING.include?(char)
      result += %|
;; "#{char}"
(setq #{binding} #{value})\n|
    end

    binding = "vemv/shortcuts/#{scope}/S-#{REPLACEMENTS[char]}"
    value = scope == 'global' ? informative_stub(binding) : "vemv/shortcuts/global/S-#{REPLACEMENTS[char]}"
    unless DUALS.include?(char)
      result += %|
;; "#{super_combination_for char}"
(setq #{binding} #{value})\n|
    end

  end

  %w(primary secondary tertiary).each do |modifier|
    (('a'..'z').to_a + (0..9).to_a.map(&:to_s) + SPECIAL).each do |char|

      next if char.include?('[f')
      next if NO_C.include?(char) && modifier_mappings[modifier] == 'C'

      binding = "vemv/shortcuts/#{scope}/#{modifier}-#{REPLACEMENTS[char]}"
      value = scope == 'global' ? informative_stub(binding) : "vemv/shortcuts/global/#{modifier}-#{REPLACEMENTS[char]}"
      result += %|
;; "#{modifier_mappings[modifier]}-#{char}"
(setq #{binding} #{value})\n|

      if modifier == 'primary'
        binding = "vemv/shortcuts/#{scope}/#{modifier}-secondary-#{REPLACEMENTS[char]}"
        value = scope == 'global' ? informative_stub(binding) : "vemv/shortcuts/global/#{modifier}-secondary-#{REPLACEMENTS[char]}"
        result += %|
;; "#{modifier_mappings[modifier]}-M-#{char}"
(setq #{binding} #{value})\n|
      end

      binding = "vemv/shortcuts/#{scope}/#{modifier}-S-#{REPLACEMENTS[char]}"
      value = scope == 'global' ? informative_stub(binding) : "vemv/shortcuts/global/#{modifier}-S-#{REPLACEMENTS[char]}"
      unless DUALS.include?(char)
        result += %|
;; "#{modifier_mappings[modifier]}-S-#{char}"
(setq #{binding} #{value})\n|
      end
    end
  end

  File.write "lib/non-submodules/vemv.shortcuts.#{scope}.base.el", result

end

def emit_bindings scope: 'global', modifier_mappings: {"primary" => 'C', "secondary" => 'M', "tertiary" => 's'}, variable_name: 'vemv/global-key-bindings', intro: false

  result = ''

  if intro
    result += %|(require 'vemv.lang)
(provide 'vemv.data.bindings)
(require 'vemv.shortcuts.global)
(require 'vemv.shortcuts.clojure)
(require 'vemv.shortcuts.ruby)
;; generated with gen.rb

(setq vemv/exhaustive-list-of-bindings-to-remove (list|

    spaces = "\n                                                       "

    SPECIAL.each do |char|
      left = char.include?('[f') ? "#{char}" : %|"#{char}"|
      if !SELF_INSERTING.include?(char) || [';'].include?(char)
        result += %|#{spaces}#{left}|
      end
    end

    %w(primary secondary tertiary).each do |modifier|
      (('a'..'z').to_a + (0..9).to_a.map(&:to_s) + SPECIAL).each do |char|
        next if char.include?('[f')
        next if NO_C.include?(char) && modifier_mappings[modifier] == 'C'
        result += %|#{spaces}"#{modifier_mappings[modifier]}-#{char}"|
        result += %|#{spaces}"#{modifier_mappings[modifier]}-S-#{char}"|
        if modifier == 'primary'
          result += %|#{spaces}"#{modifier_mappings[modifier]}-M-#{char}"|
        end
      end
    end

    result += %|))|
  end

  result += %|

(setq #{variable_name}
  (vemv/hash-map
|

  SPECIAL.each do |char|
    command = "vemv/shortcuts/#{scope}/#{REPLACEMENTS[char]}"
    left = char.include?('[f') ? "#{char}" : %|"#{char}"|
    result += %|    #{left} (argless (if #{command} (vemv/keyboard-funcall #{command})))\n| unless SELF_INSERTING.include?(char)
    unless DUALS.include?(char)
      command = "vemv/shortcuts/#{scope}/S-#{REPLACEMENTS[char]}"
      result += %|    "#{super_combination_for char}" (argless (if #{command} (vemv/keyboard-funcall #{command})))\n|
    end
  end

  %w(primary secondary tertiary).each do |modifier|
    (('a'..'z').to_a + (0..9).to_a.map(&:to_s) + SPECIAL).each do |char|
      next if char.include?('[f')
      next if NO_C.include?(char) && modifier_mappings[modifier] == 'C'
      command = "vemv/shortcuts/#{scope}/#{modifier}-#{REPLACEMENTS[char]}"
      result += %|    "#{modifier_mappings[modifier]}-#{char}" (argless (if #{command} (vemv/keyboard-funcall #{command})))\n|
      if modifier == 'primary'
        command = "vemv/shortcuts/#{scope}/#{modifier}-secondary-#{REPLACEMENTS[char]}"
        result += %|    "#{modifier_mappings[modifier]}-M-#{char}" (argless (if #{command} (vemv/keyboard-funcall #{command})))\n|
      end
      s_command = "vemv/shortcuts/#{scope}/#{modifier}-S-#{REPLACEMENTS[char]}"
      result += %|    "#{modifier_mappings[modifier]}-S-#{char}" (argless (if #{s_command} (vemv/keyboard-funcall #{s_command})))\n|
    end
  end

  result += %|))|

  fname = 'lib/non-submodules/vemv.data.bindings.el'

  if intro
    File.write fname, result
  else
    File.write fname, result, File.size(fname), mode: 'a'
  end

end

def emit_to_remove scope='global', modifier_mappings: {"primary" => 'C', "secondary" => 'M', "tertiary" => 's'}
  result = (

      [(SPECIAL.map do |char|
        r = []
        left = char.include?('[f') ? "#{char}" : %|"#{char}"|
        r << left
        r << super_combination_for(char)
        r
      end),

       (%w(primary secondary tertiary).map do |modifier|
         (('a'..'z').to_a + (0..9).to_a.map(&:to_s) + SPECIAL).map do |char|
           next if char.include?('[f')
           r = [%|"#{modifier_mappings[modifier]}-#{char}"|, %|"#{modifier_mappings[modifier]}-S-#{char}"|]
         end
       end)]

  ).flatten.compact.map{|x| x.gsub(/^\"C/, "\"\\C").gsub(/^\"M/, "\"\\M") }.map{|a| !a.include?('"') && !a.include?("[f") ? %|"#{a}"| : a }.reject{|a| a.include? "<" }
  puts result.join(" ")
end

emit_setqs
emit_setqs scope: 'clojure'
emit_setqs scope: 'ruby'
emit_bindings intro: true
emit_bindings scope: 'clojure', variable_name: 'vemv/clojure-key-bindings'
emit_bindings scope: 'ruby', variable_name: 'vemv/ruby-key-bindings'
