# Certain C-<char> keybindings are forbidden
NO_C = %w([ i m g)

# there's no C-S-semicolon, but C-colon etc
DUALS = %w(` ~ + = [ ] { } | \\ ; : ' " , < . > / ? ! @ # $ % ^ & * ( ) - _ 1 2 3 4 5 6 7 8 9 0)

# XXX add all possible members of DUALS here
# no backlash for now - don't want to escape things
REPLACEMENTS = Hash.new{|map, key| key }.merge({
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

def emit_setqs scope: 'global', modifier_mappings: {"primary" => 'C', "secondary" => 'M', "tertiary" => 's'}
  
  result = "(provide 'vemv.shortcuts.global.base)\n;; generated with gen.rb\n"
  
  SPECIAL.each do |char|
    
    result += %|
;; "#{char}"
(setq vemv/shortcuts/#{scope}/#{REPLACEMENTS[char]} nil)\n|

    result += %|
;; "S-#{char}"
(setq vemv/shortcuts/#{scope}/S-#{REPLACEMENTS[char]} nil)\n| unless (char.include?('[f') || DUALS.include?(char))
    
  end
  
  %w(primary secondary tertiary).each do |modifier|
    (('a'..'z').to_a + (0..9).to_a.map(&:to_s) + SPECIAL).each do |char|
      
      next if char.include?('[f')
      next if NO_C.include?(char) && modifier_mappings[modifier] == 'C'
      
      result += %|
;; "#{modifier_mappings[modifier]}-#{char}"
(setq vemv/shortcuts/#{scope}/#{modifier}-#{REPLACEMENTS[char]} nil)\n|
      
      result += %|
;; "#{modifier_mappings[modifier]}-S-#{char}"
(setq vemv/shortcuts/#{scope}/#{modifier}-S-#{REPLACEMENTS[char]} nil)\n| unless DUALS.include?(char)
    end
  end
  
  File.write 'lib/non-submodules/vemv.shortcuts.global.base.el', result
  
end

def emit_bindings scope='global', modifier_mappings: {"primary" => 'C', "secondary" => 'M', "tertiary" => 's'}
  
  result = %|(require 'vemv.lang)
(provide 'vemv.data.bindings)
(require 'vemv.shortcuts.global)
(require 'vemv.shortcuts.clojure)
;; generated with gen.rb

(setq vemv/global-key-bindings
  (vemv/hash-map
|
  
  SPECIAL.each do |char|
    command = "vemv/shortcuts/#{scope}/#{REPLACEMENTS[char]}"
    left = char.include?('[f') ? "#{char}" : %|"#{char}"|
    result += %|    #{left} (argless (if #{command} (funcall #{command})))\n|
    unless char.include?('[f') || DUALS.include?(char)
      command = "vemv/shortcuts/#{scope}/S-#{REPLACEMENTS[char]}"
      result += %|    "S-#{char}" (argless (if #{command} (funcall #{command})))\n|
    end
  end
  
  %w(primary secondary tertiary).each do |modifier|
    (('a'..'z').to_a + (0..9).to_a.map(&:to_s) + SPECIAL).each do |char|
      next if char.include?('[f')
      next if NO_C.include?(char) && modifier_mappings[modifier] == 'C'
      command = "vemv/shortcuts/#{scope}/#{modifier}-#{REPLACEMENTS[char]}"
      result += %|    "#{modifier_mappings[modifier]}-#{char}" (argless (if #{command} (funcall #{command})))\n|
      s_command = "vemv/shortcuts/#{scope}/#{modifier}-S-#{REPLACEMENTS[char]}"
      result += %|    "#{modifier_mappings[modifier]}-S-#{char}" (argless (if #{s_command} (funcall #{s_command})))\n|
    end
  end
  
  result += %|))|
  
  File.write 'lib/non-submodules/vemv.data.bindings.el', result
  
end

def emit_to_remove scope='global', modifier_mappings: {"primary" => 'C', "secondary" => 'M', "tertiary" => 's'}
  result = (
    
      [(SPECIAL.map do |char|
        r = []
        left = char.include?('[f') ? "#{char}" : %|"#{char}"|
        r << left
        unless char.include?('[f')
          r << "S-#{char}"
        end
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
emit_bindings