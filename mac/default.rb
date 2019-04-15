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
    command "code --install-extension #{extension}"
    not_if "code --list-extensions | grep #{extension}"
  end
end

packages = ['awscli',
            'cloc',
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
            'terraform',
            'the_silver_searcher',
            'tree',
            'vim']

casks = ['battle-net',
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
         'virtualbox',
         'virtualbox-extension-pack',
         'vlc']

go_packages = ['github.com/kisielk/errcheck',
               'github.com/nsf/gocode',
               'github.com/rogpeppe/godef']

vscode_exentsions = ['lfs.vscode-emacs-friendly',
                     'ms-python.python',
                     'rebornix.ruby']

execute 'disable homebrew analytics' do
  command 'brew analytics off'
  not_if 'brew analytics state | grep disabled'
end

packages.each { |p| package p }
casks.each { |c| cask c }
go_packages.each { |p| go_package p }
vscode_exentsions.each { |e| vscode_exentsion e }

# TODO: symlink dotfiles instead of copying them
remote_file '/Users/jrab89/.pryrc'
remote_file '/Users/jrab89/.psqlrc'
remote_file '/Users/jrab89/.zshrc'
remote_file '/Users/jrab89/.vimrc'
remote_file '/Users/jrab89/Library/Application Support/Code/User/settings.json'
remote_file '/Users/jrab89/Library/Application Support/Code/User/keybindings.json'
