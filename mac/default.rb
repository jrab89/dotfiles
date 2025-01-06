# TODO: installation of Homebrew and oh-my-zsh is manual

# TODO: use https://github.com/k0kubun/itamae-plugin-resource-cask ?
define :cask do
  cask_name = params[:name]
  execute "install #{cask_name}" do
    command "brew install --cask '#{cask_name}'"
    not_if "test -e $(brew --prefix)/Caskroom/#{cask_name}"
  end
end

define :vscode_exentsion do
  extension = params[:name]
  execute "installing vscode extension #{extension}" do
    command "code --install-extension #{extension}"
    not_if "code --list-extensions | grep #{extension}"
  end
end

define :ruby_install do
  version = params[:name]
  execute "installing ruby #{version}" do
    command "ruby-install ruby '#{version}'"
    not_if "test -e $HOME/.rubies/ruby-#{version}"
  end
end

define :pyenv_install do
  version = params[:name]
  execute "installing python #{version}" do
    command "pyenv install '#{version}'"
    not_if "test -e $HOME/.pyenv/versions/#{version}"
  end
end

USER = run_command('whoami').stdout.chomp
LATEST_RUBY_VERSION = run_command('curl -sS https://raw.githubusercontent.com/postmodern/ruby-versions/master/ruby/stable.txt | tail -n 1').stdout.chomp
MAC_DIR = File.expand_path(File.dirname(__FILE__))
LATEST_PYTHON_VERSION = JSON.parse(run_command("curl -sS 'https://api.github.com/repos/python/cpython/tags'").stdout)
                            .map{ |tag| tag["name"] }
                            .select { |name| name =~ /\d+\.\d+\.\d+$/ }
                            .map { |name| name.gsub("v", "") }
                            .sort
                            .reverse
                            .first

# TODO: use asdf (or what https://omakub.org/ uses?) instead of language specific tools

PACKAGES = ['awscli',
            'bat',
            'chruby',
            'cloc',
            'docker-compose',
            'docker',
            'git',
            'go',
            'graphviz',
            'htop',
            'ispell',
            'jq',
            'kubernetes-cli',
            'mysql',
            'node',
            'openjdk',
            'p7zip',
            'pipx',
            'pyenv',
            'ruby-install',
            'ruby',
            'shellcheck',
            'the_silver_searcher',
            'tree'].freeze

CASKS = ['caffeine',
         'ghostty',
         'licecap',
         'visual-studio-code'].freeze

VSCODE_EXENTSIONS = ['golang.go',
                     'lfs.vscode-emacs-friendly',
                     'ms-python.vscode-pylance',
                     'timonwong.shellcheck'].freeze

execute 'disable homebrew analytics' do
  command 'brew analytics off'
  not_if 'brew analytics state | grep disabled'
end

PACKAGES.each { |package_name| package package_name }
CASKS.each { |cask_name| cask cask_name }
VSCODE_EXENTSIONS.each { |extension_name| vscode_exentsion extension_name }

ruby_install LATEST_RUBY_VERSION
pyenv_install LATEST_PYTHON_VERSION

link "/Users/#{USER}/.pryrc" do
  to "#{MAC_DIR}/files/.pryrc"
  force true
end

link "/Users/#{USER}/.pgpass" do
  to "#{MAC_DIR}/files/.pgpass"
  force true
end

link "/Users/#{USER}/.psqlrc" do
  to "#{MAC_DIR}/files/.psqlrc"
  force true
end

link "/Users/#{USER}/.vimrc" do
  to "#{MAC_DIR}/files/.vimrc"
  force true
end

link "/Users/#{USER}/Library/Application Support/Code/User/settings.json" do
  to "#{MAC_DIR}/files/settings.json"
  force true
end

link "/Users/#{USER}/Library/Application Support/Code/User/keybindings.json" do
  to "#{MAC_DIR}/files/keybindings.json"
  force true
end

template "/Users/#{USER}/.zshrc" do
  variables ruby_version: LATEST_RUBY_VERSION,
            python_version: LATEST_PYTHON_VERSION
end
