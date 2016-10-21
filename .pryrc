Pry.config.correct_indent = false if ENV['INSIDE_EMACS']
Pry.color = true
Pry.config.editor = 'emacsclient --tty --quiet'