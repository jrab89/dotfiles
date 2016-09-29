#!/bin/bash

green='\e[1;32m'
red='\e[1;31m'
color_off='\e[0m'

brew_packages="
  awscli
  cloc
  docker-compose
  git
  graphviz
  homebrew/completions/brew-cask-completion
  homebrew/completions/bundler-completion
  homebrew/completions/docker-completion
  homebrew/completions/docker-compose-completion
  homebrew/completions/gem-completion
  homebrew/completions/pip-completion
  homebrew/completions/rails-completion
  homebrew/completions/rake-completion
  homebrew/completions/ruby-completion
  homebrew/completions/vagrant-completion
  htop
  ispell
  jq
  jruby
  mysql
  node
  p7zip
  python
  ruby
  shellcheck
  terraform
  the_silver_searcher
  tree"

brew_cask_packages="
  battle-net
  caffeine
  docker
  emacs
  firefox
  google-chrome
  iterm2
  java
  kitematic
  plex-media-server
  sequel-pro
  slack
  transmission
  vagrant
  virtualbox
  virtualbox-extension-pack
  vlc"


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
