# TODO: `File.absolute_path` vs `File.expand_path`?
SCRIPT_DIR = File.absolute_path(File.dirname(__FILE__))
HOME_DIR = Dir.home
DONT_SYMLINK = ['.git', '.gitignore']

module Tty
  module_function

  def blue
    bold 34
  end

  def red
    bold 31
  end

  def reset
    escape 0
  end

  def bold(n = 39)
    escape "1;#{n}"
  end

  def underline
    escape "4;39"
  end

  def escape(n)
    "\e[#{n}m" if STDOUT.tty?
  end
end

class Array
  def shell_s
    cp = dup
    first = cp.shift
    cp.map { |arg| arg.gsub " ", "\\ " }.unshift(first).join(" ")
  end
end

def info(message)
  puts "#{Tty.blue}Info:#{Tty.bold} #{message}#{Tty.reset}"
end

def warn(warning)
  puts "#{Tty.red}Warning:#{Tty.bold} #{warning}#{Tty.reset}"
end

def has_executable?(name)
  Kernel.system('/usr/bin/which', '-s', name)
end

def system(*args)
  abort "Failed during: #{args.shell_s}" unless Kernel.system(*args)
end

if has_executable?('brew')
  info 'brew already installed'
else
  info 'brew not installed, installing...'
  system 'curl', '--fail', '--silent', '--show-error', '--location',
         'https://raw.githubusercontent.com/Homebrew/install/master/install',
         '-o', '/tmp/install_brew.rb'
  system '/usr/bin/ruby', '/tmp/install_brew.rb'
  info 'brew installed successfully!'
end

system 'brew', 'analytics', 'off'

info "setting up symlinks in #{HOME_DIR}"
symlink_basenames = Dir.glob('.[^.]*', base: SCRIPT_DIR) - DONT_SYMLINK
abs_symlink_targets = symlink_basenames.map { |basename| File.join(SCRIPT_DIR, basename) }
abs_symlink_sources = symlink_basenames.map { |basename| File.join(HOME_DIR, basename) }

abs_symlink_sources.zip(abs_symlink_targets, symlink_basenames).each do |source, target, basename|
  if File.symlink?(source) && File.readlink(source) == target
    info "symlink for #{basename} already exists"
  elsif File.symlink?(source)
    warn "symlink #{source} already exists, but doesn't point to #{target}"
  elsif File.exist?(source)
    warn "file already exists at #{source}"
  else
    File.symlink(target, source)
    info "made symlink #{basename}"
  end
end

info 'installing brew packages...'
system 'brew', 'bundle', 'install', '--global', '--no-upgrade'
info 'all done!'

# TODO: install ruby 2.x with `ruby-install`
#       move existing symlinks and files
