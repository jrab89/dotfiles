#!/bin/bash

current_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

green='\e[1;32m'
red='\e[1;31m'
color_off='\e[0m'

brew_packages="
  awscli
  cloc
  docker-compose
  git
  graphviz
  htop
  ispell
  jq
  mysql
  node
  p7zip
  python
  shellcheck
  terraform
  the_silver_searcher
  tree"

brew_cask_packages="
  caffeine
  docker
  emacs
  iterm2
  java
  kitematic
  slack
  vagrant
  virtualbox
  virtualbox-extension-pack
  vlc"

symlink_to_home_dir="
  .vimrc
  .pryrc
  .zshrc
  .emacs.d"

print_status() {
    printf " --> ${green}$1${color_off}\n"
}

die() {
    printf " --> ${red}$1${color_off}\n"
    exit 1
}

command_exists() {
    if command -v "$1" > /dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

# install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

if ! command_exists brew; then
    print_status "installing homebrew"
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
else
    print_status "homebrew already installed"
fi

brew analytics off

print_status "installing homebrew packages..."
for package in $brew_packages; do
    brew install "$package"
        if [ "$?" -ne 0 ]; then
            die "failed to install $package"
        fi
done

print_status "installing brew cask packages..."
for package in $brew_cask_packages; do
    brew cask install "$package"
        if [ "$?" -ne 0 ]; then
            die "failed to install $package"
        fi
done

# TODO: don't die if symlink already exists
print_status "symlinking dotfiles to home dir"
for to_symlink in $symlink_to_home_dir; do
    ln -s "$current_dir/$to_symlink" "$HOME/$to_symlink"
        if [ "$?" -ne 0 ]; then
            die "failed to symlink $HOME/$to_symlink to $current_dir/$to_symlink"
        fi
done
