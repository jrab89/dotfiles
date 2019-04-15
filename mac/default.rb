# TODO: use https://github.com/k0kubun/itamae-plugin-resource-cask ?
define :cask do
  cask_name = params[:name]
  execute "install #{cask_name}" do
    command "brew cask install '#{cask_name}'"
    not_if "test -e $(brew --prefix)/Caskroom/#{cask_name}"
  end
end

define :go_package do
  package = params[:name]
  execute "install go package #{package}" do
    command "go get '#{package}'"
    not_if "go list all | grep #{package}"
  end
end

define :vscode_exentsion do
  extension = params[:name]
  execute "installing vscode extension #{extension}" do
    command "vscodium --install-extension #{extension}"
    not_if "vscodium --list-extensions | grep #{extension}"
  end
end

define :ruby_install do
  version = params[:name]
  execute "installing ruby #{version}" do
    command "ruby-install ruby '#{version}'"
    not_if "test -e $HOME/.rubies/ruby-#{version}"
  end
end

USER = run_command('whoami').stdout.chomp
LATEST_RUBY_VERSION = run_command('curl -sS https://raw.githubusercontent.com/postmodern/ruby-versions/master/ruby/stable.txt | tail -n 1').stdout.chomp

PACKAGES = ['awscli',
            'cloc',
            'chruby',
            'ruby-install',
            'docker-compose',
            'git',
            'go',
            'graphviz',
            'htop',
            'ispell',
            'jq',
            'mysql',
            'node',
            'p7zip',
            'python',
            'python3',
            'ruby',
            'shellcheck',
            # TODO: install terraform 0.11.7
            # 'terraform',
            'the_silver_searcher',
            'tree',
            'vim']

CASKS = ['battle-net',
         'blender',
         'caffeine',
         'docker',
         'emacs',
         'gimp',
         'iterm2',
         'java8',
         'licecap',
         'slack',
         'steam',
         'transmission',
         'vagrant',
         'vscodium',
         'virtualbox',
         'virtualbox-extension-pack',
         'vlc']

GO_PACKAGES = ['github.com/kisielk/errcheck',
               'github.com/nsf/gocode',
               'github.com/rogpeppe/godef']

VSCODE_EXENTSIONS = ['lfs.vscode-emacs-friendly',
                     'ms-python.python',
                     'rebornix.ruby',
                     'timonwong.shellcheck',
                     'mauve.terraform']

execute 'disable homebrew analytics' do
  command 'brew analytics off'
  not_if 'brew analytics state | grep disabled'
end

PACKAGES.each { |package_name| package package_name }
CASKS.each { |cask_name| cask cask_name }
GO_PACKAGES.each { |package_name| go_package package_name }
VSCODE_EXENTSIONS.each { |extension_name| vscode_exentsion extension_name }

ruby_install LATEST_RUBY_VERSION

remote_file "/Users/#{USER}/.pryrc"
remote_file "/Users/#{USER}/.psqlrc"
remote_file "/Users/#{USER}/.vimrc"
remote_file "/Users/#{USER}/Library/Application Support/VSCodium/User/settings.json"
remote_file "/Users/#{USER}/Library/Application Support/VSCodium/User/keybindings.json"

template "/Users/#{USER}/.zshrc" do
  variables(ruby_version: LATEST_RUBY_VERSION)
end
