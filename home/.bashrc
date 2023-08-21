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
#source $HOME/bash_completion.d/gradle-completion.bash

### Java Environment Variables
# export JAVA_HOME="/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.191.b12-1.el7_6.x86_64/"
# export JAVA_BIN="$JAVA_HOME/bin"

export JAVA_HOME=/usr/lib/jvm/java-11-openjdk
export PATH=$JAVA_HOME/bin:$PATH 

### The all important path
export PATH=$PATH:/usr/lib64/qt-3.3/bin:/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:~/bin:/bin #:$JAVA_BIN

### Used for multiple python versions
# export PATH="~/.pyenv/bin:$PATH"
# eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"

### History format
HISTTIMEFORMAT=' %a %B %d | [%T] | '

export PATH=~/.npm-global/bin:$PATH
export PATH=/opt/ninja:$PATH
export PATH=$HOME/.npm/bin:$PATH

source /opt/rh/devtoolset-10/enable
export CMAKE_C_COMPILER=/opt/rh/devtoolset-10/root/bin/gcc
export CMAKE_CXX_COMPILER=/opt/rh/devtoolset-10/root/bin/g++
export PATH=/opt/rh/devtoolset-10/root/bin/:$PATH
export PATH=/opt/cmake/bin:$PATH

# Typescript
export DENO_INSTALL="/home/jstoup/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"



##########################
# ALIASES
##########################
alias resource='source ~/.bashrc'
alias userlist="eval getent passwd {$(awk '/^UID_MIN/ {print $2}' /etc/login.defs)..$(awk '/^UID_MAX/ {print $2}' /etc/login.defs)} | cut -d: -f1"
alias chownjs="sudo chown jstoup:jstoup *"
alias less='less -R' #need this so colorized text displays correctly

alias im='cd ~/ion-suite/modules/'
alias sid='ssh ion_dev@james-dev-server'
alias less='less -R'
alias ls='ls --color=auto'

### grepsmall returns just the salient value pulled from grep, not the entire file
function _grepsmall {
    grep -E -o ".{0,30}$1.{0,30}" * | sed 's/:/:  /'
}
alias grepsmall='_grepsmall'

alias build-mssv="cd ${HOME}/repos/mssv/ ; ./mvnw clean install -P dist -DskipTests=true"
alias start-mssv="cd ${HOME}/repos/mssv/dist ; chmod a+x start.sh; ./start.sh; cd -"

alias build-webumc="${HOME}/repos/developer_tools/webumc_tools/build-webumc.sh -s"
alias build-webumc-clean="${HOME}/repos/developer_tools/webumc_tools/build-webumc.sh -cf"
alias build-webumc-backend="${HOME}/repos/developer_tools/webumc_tools/build-webumc.sh -b"
alias start-webumc="cd ${HOME}/repos/web-umc-new-main/server/build/distributions/ ; tar -xzf *.tgz; ./web-umc-server-6.6.0.0/start-web-umc-server.sh; cd -"


##########################
# GIT PROMPT
##########################

### Enhancing bash for git usage
function _git_prompt() {
    local git_status="`git status -unormal 2>&1`"
    #if ! [[ "$git_status" =~ Not\ a\ git\ repo ]]; then
    if ! [[ "$git_status" =~ fatal:\ not\ a\ git\ repo ]]; then # must have for newer versions of git
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
    PS1="`_git_prompt`"'\[\033[1;91m\](\u - \t)\[\033[0;36m\]\n  \[\033[1;36m\][\w]\[\033[0;97m\]\$ \[\e[00m\]'
}

PROMPT_COMMAND=_prompt_command

#export PS1="[\u@\h \w]$ "

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
