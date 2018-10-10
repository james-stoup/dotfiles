# .bashrc

### Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi


### Homeshick
source "$HOME/.homesick/repos/homeshick/homeshick.sh"
source "$HOME/.homesick/repos/homeshick/completions/homeshick-completion.bash"

### Bash auto completion
source /etc/profile.d/bash_completion.sh

### Gradle auto completion
source $HOME/bash_completion.d/gradle-completion.bash

### Git auto completion
source ~/.git-completion.bash

### Go Environment Variables
export GOPATH=~/go
export GOBIN=$GOPATH/bin
export GOROOT=/usr/local/go

### Java
export JAVA_BIN=/usr/java/latest/bin

export PATH=$PATH:/usr/lib64/qt-3.3/bin:/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:~/bin:$GOROOT/bin:/bin:$GOBIN:$JAVA_BIN

### Java Environment Variables
export JAVA_HOME="/usr/java/latest"

### aliases 
alias resource='source ~/.bashrc'
alias im='cd ~/ion-suite/modules/'
alias sid='ssh ion_dev@james-dev-server'

### Print the test coverage stats for each function in Go
function _gofuncstats {
    go tool cover -func="$1"
}
alias gofuncstats='_gofuncstats'


### grepsmall returns just the salient value pulled from grep, not the entire file
function _grepsmall {
    grep -E -o ".{0,30}$1.{0,30}" *
}
alias grepsmall='_grepsmall'


### Enhancing bash for git usage
function _git_prompt() {
    local git_status="`git status -unormal 2>&1`"
    if ! [[ "$git_status" =~ Not\ a\ git\ repo ]]; then
        if [[ "$git_status" =~ nothing\ to\ commit ]]; then
            local ansi=42
        elif [[ "$git_status" =~ nothing\ added\ to\ commit\ but\ untracked\ files\ present ]]; then
            local ansi=43
        else
            local ansi=41
        fi
        if [[ "$git_status" =~ On\ branch\ ([^[:space:]]+) ]]; then
            branch=${BASH_REMATCH[1]}
            test "$branch" != master || branch=' '
        else
            # Detached HEAD.  (branch=HEAD is a faster alternative.)
            branch="(`git describe --all --contains --abbrev=4 HEAD 2> /dev/null ||
                    echo HEAD`)"
        fi
        echo -n '\[\e[0;37;'"$ansi"';1m\]'"$branch"'\[\e[0m\] '
    fi
}

function _prompt_command() {
    PS1="`_git_prompt`"'(\[\033[0;31m\]\u - \t\[\033[0;30m\])\n  [\[\033[0;34m\]\w\[\033[0;30m\]]\$ '
}

PROMPT_COMMAND=_prompt_command


# Used for multiple python versions
export PATH="~/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/jstoup/.sdkman"
[[ -s "/home/jstoup/.sdkman/bin/sdkman-init.sh" ]] && source "/home/jstoup/.sdkman/bin/sdkman-init.sh"

