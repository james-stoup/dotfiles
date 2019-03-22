# .bashrc

### Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

##########################
# VARIABLES & SOURCING
##########################

### Homeshick
source "$HOME/.homesick/repos/homeshick/homeshick.sh"
source "$HOME/.homesick/repos/homeshick/completions/homeshick-completion.bash"

### Bash auto completion
source /etc/profile.d/bash_completion.sh

### Gradle auto completion
source $HOME/bash_completion.d/gradle-completion.bash

### Java Environment Variables
export JAVA_HOME="/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.191.b12-1.el7_6.x86_64/"
export JAVA_BIN="$JAVA_HOME/bin"

### The all important path
export PATH=$PATH:/usr/lib64/qt-3.3/bin:/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:~/bin:/bin:$JAVA_BIN

### Used for multiple python versions
export PATH="~/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"


##########################
# ALIASES
##########################
alias resource='source ~/.bashrc'
alias userlist="eval getent passwd {$(awk '/^UID_MIN/ {print $2}' /etc/login.defs)..$(awk '/^UID_MAX/ {print $2}' /etc/login.defs)} | cut -d: -f1"
alias chownjs="sudo chown jstoup:jstoup *"
alias less='less -R' #need this so colorized text displays correctly

alias im='cd ~/ion-suite/modules/'
alias sid='ssh ion_dev@james-dev-server'
alias sir='ssh jstoup@rpm-server'
alias isd='cd /home/jstoup/ION/ion-suite'
alias iwd='cd /home/jstoup/ION/ion-websites'

### grepsmall returns just the salient value pulled from grep, not the entire file
function _grepsmall {
    grep -E -o ".{0,30}$1.{0,30}" * | sed 's/:/:  /'
}
alias grepsmall='_grepsmall'


##########################
# GIT PROMPT
##########################

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




