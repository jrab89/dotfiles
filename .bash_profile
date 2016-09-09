# source /usr/local/etc/bash_completion.d/docker
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
    local color_off='\e[0m'

    local user_and_dir="\u \[${cyan}\]\w\[${color_off}\]"

    if [ "$AWS_DEFAULT_PROFILE" == "" ]; then
        local current_aws_profile=""
    else
        local current_aws_profile="\[${yellow}\]${AWS_DEFAULT_PROFILE}\[${color_off}\] "
    fi

    local current_git_branch
    if [ "$(parse_git_branch)" == "" ]; then
        current_git_branch=""
    else
        current_git_branch="\[${blue}\]$(parse_git_branch)\[${color_off}\] "
    fi

    local exit_code_color
    if [ $last_exit != 0 ]; then
        exit_code_color=$red
    else
        exit_code_color=$green
    fi
    local colored_last_exit="\[${exit_code_color}\]${last_exit}\[${color_off}\]"

    PS1="${user_and_dir} ${current_aws_profile}${current_git_branch}${colored_last_exit} $ "
}

PROMPT_COMMAND=set_bash_prompt

# https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa
