source /usr/local/etc/bash_completion.d/docker
source /usr/local/etc/bash_completion.d/brew
source /usr/local/etc/bash_completion.d/brew-cask
source /usr/local/etc/bash_completion.d/bundler
source /usr/local/etc/bash_completion.d/docker-compose
source /usr/local/etc/bash_completion.d/gem
source /usr/local/etc/bash_completion.d/git-completion.bash
source /usr/local/etc/bash_completion.d/npm
source /usr/local/etc/bash_completion.d/pip
source /usr/local/etc/bash_completion.d/rails
source /usr/local/etc/bash_completion.d/rake
source /usr/local/etc/bash_completion.d/ruby
source /usr/local/etc/bash_completion.d/vagrant

source ~/.bash_env

## https://coderwall.com/p/fasnya/add-git-branch-name-to-bash-promp
# $ git branch
#   asdf
#   bar
#   foo
# * master
# $ git branch 2> /dev/null | grep '^\*' | sed -e 's/^\*//'
# master
parse_git_branch() {
    git branch 2> /dev/null | grep '^\*' | sed -e 's/^\*[ \t]*//'
}

parse_current_venv() {
    basename "$(dirname "$VIRTUAL_ENV")"
}

# http://stackoverflow.com/a/16715681
set_bash_prompt() {
    # http://unix.stackexchange.com/a/105932
    # wrap characters that should not be counted in the prompt's length in \[ \]
    # "user pwd aws_default_profile current_git_branch last_exit_code $"
    # "jrabovsky ~/git_repos/busboy ghprod master 0 $"

    local last_exit="$?"

    local blue='\e[1;34m'
    local cyan='\e[1;36m'
    local green='\e[1;32m'
    local red='\e[1;31m'
    local yellow='\e[1;33m'
    local magenta='\e[1;35m'
    local color_off='\e[0m'

    local user_and_dir="\u \[${cyan}\]\w\[${color_off}\]"

    if [ "$AWS_DEFAULT_PROFILE" == "" ]; then
        local current_aws_profile=""
    else
        local current_aws_profile="\[${yellow}\]aws:${AWS_DEFAULT_PROFILE}\[${color_off}\] "
    fi

    local current_git_branch
    if [ "$(parse_git_branch)" == "" ]; then
        current_git_branch=""
    else
        local status_icon
        if [ "$(git status -s)" == "" ]; then
            status_icon="\[${green}\]o\[${color_off}\]"
        else
            status_icon="\[${red}\]x\[${color_off}\]"
        fi

        current_git_branch="\[${blue}\]git:$(parse_git_branch)(${status_icon}\[${blue}\])\[${color_off}\] "
    fi

    local current_venv
    if [ "$VIRTUAL_ENV" == "" ]; then
        current_venv=""
    else
        current_venv="\[${magenta}\]venv:$(parse_current_venv)\[${color_off}\] "
    fi

    local exit_code_color
    if [ $last_exit != 0 ]; then
        exit_code_color=$red
    else
        exit_code_color=$green
    fi
    local colored_last_exit="\[${exit_code_color}\]${last_exit}\[${color_off}\]"

    PS1="${user_and_dir} ${current_aws_profile}${current_venv}${current_git_branch}${colored_last_exit} $ "
}

awsp() {
    export AWS_DEFAULT_PROFILE="$1"
    export AWS_PROFILE="$1"
}

PROMPT_COMMAND=set_bash_prompt

export AWS_DEFAULT_PROFILE=sandbox

# https://www.digitalocean.com/community/tutorials/how-to-use-bash-history-commands-and-expansions-on-a-linux-vps
export HISTSIZE=1000000
export HISTFILESIZE=1000000

shopt -s histappend

complete -C '/usr/local/bin/aws_completer' aws

alias ll='ls -althrG'
alias tf=terraform
alias myip='curl http://checkip.amazonaws.com'
alias be='bundle exec'
alias ec='emacsclient --tty --quiet'
alias now='date -u +"%Y-%m-%dT%H:%M:%SZ"'

export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$HOME/.emacs.d/vendor/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"

source ~/.rvm/scripts/rvm

# https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/
# eval "$(ssh-agent -s)"
# ssh-add ~/.ssh/id_rsa
