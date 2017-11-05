# XXX these commands should File.write for greater automation / bisecting

# Certain C-<char> keybindings are forbidden
NO_C = %w([ i m g)

# there's no C-S-semicolon, but C-colon etc
DUALS = %w(` ~ + = [ ] { } | \\ ; : ' " , < . > / ? ! @ # $ % ^ & * ( ) - _ 1 2 3 4 5 6 7 8 9 0)

# XXX add all possible members of DUALS here
# no backlash for now - don't want to escape things
REPLACEMENTS = Hash.new{|map, key| key }.merge({
  "`" => 'backtick',
  ";" => 'semicolon',
  ":" => 'colon',
  "[" => 'left-bracket',
  "]" => 'right-bracket',
  "{" => 'left-curly',
  "}" => 'right-curly',
  "|" => 'bar',
  "'" => 'single-quote',
  "," => 'comma',
  "." => 'dot',
  "/" => 'slash',
  "?" => 'question-mark',
  "=" => 'equal',
  "+" => 'plus',
  "<next>" => "next",
  "<prior>" => "prior",
  "<end>" => "end",
  "<home>" => "home",
  "<up>" => "up",
  "<down>" => "down",
  "<left>" => "left",
  "<right>" => "right",
  "RET" => "RET", # return key
  "SPC" => "SPC", # space key
  "[f1]" => "f1",
  "[f2]" => "f2",
  "[f3]" => "f3",
  "[f4]" => "f4",
  "[f5]" => "f5",
  "[f6]" => "f6",
  "[f7]" => "f7",
  "[f8]" => "f8",
  "[f9]" => "f9",
  "[f10]" => "f10",
  "[f11]" => "f11",
  "[f12]" => "f12"
})

SPECIAL = REPLACEMENTS.keys

def emit_setqs scope: 'global', modifier_mappings: {"primary" => 'C', "secondary" => 'M', "tertiary" => 's'}

  SPECIAL.each do |char|
    
    puts %|
;; "#{char}"
(setq vemv/shortcuts/#{scope}/#{REPLACEMENTS[char]} nil)|

    puts %|
;; "S-#{char}"
(setq vemv/shortcuts/#{scope}/S-#{REPLACEMENTS[char]} nil)| unless (char.include?('[f') || DUALS.include?(char))
    
  end
  
  %w(primary secondary tertiary).each do |modifier|
    (('a'..'z').to_a + (0..9).to_a.map(&:to_s) + SPECIAL).each do |char|
      
      next if char.include?('[f')
      next if NO_C.include?(char) && modifier_mappings[modifier] == 'C'
      
      puts %|
;; "#{modifier_mappings[modifier]}-#{char}"
(setq vemv/shortcuts/#{scope}/#{modifier}-#{REPLACEMENTS[char]} nil)|
      
      puts %|
;; "#{modifier_mappings[modifier]}-S-#{char}"
(setq vemv/shortcuts/#{scope}/#{modifier}-S-#{REPLACEMENTS[char]} nil)| unless DUALS.include?(char)
    end
  end

end

def emit_bindings scope='global', modifier_mappings: {"primary" => 'C', "secondary" => 'M', "tertiary" => 's'}

  SPECIAL.each do |char|
    command = "vemv/shortcuts/#{scope}/#{REPLACEMENTS[char]}"
    left = char.include?('[f') ? "#{char}" : %|"#{char}"|
    puts %|#{left} (argless (if #{command} (funcall #{command})))|
    unless char.include?('[f') || DUALS.include?(char)
      command = "vemv/shortcuts/#{scope}/S-#{REPLACEMENTS[char]}"
      puts %|"S-#{char}" (argless (if #{command} (funcall #{command})))|
    end
  end
  
  %w(primary secondary tertiary).each do |modifier|
    (('a'..'z').to_a + (0..9).to_a.map(&:to_s) + SPECIAL).each do |char|
      next if char.include?('[f')
      next if NO_C.include?(char) && modifier_mappings[modifier] == 'C'
      command = "vemv/shortcuts/#{scope}/#{modifier}-#{REPLACEMENTS[char]}"
      puts %|"#{modifier_mappings[modifier]}-#{char}" (argless (if #{command} (funcall #{command})))|
      s_command = "vemv/shortcuts/#{scope}/#{modifier}-S-#{REPLACEMENTS[char]}"
      puts %|"#{modifier_mappings[modifier]}-S-#{char}" (argless (if #{s_command} (funcall #{s_command})))|
    end
  end

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