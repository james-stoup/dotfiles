# .bashrc

### Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

##########################
# VARIABLES & SOURCING
##########################

### Homeshick
#source "$HOME/.homesick/repos/homeshick/homeshick.sh"
#source "$HOME/.homesick/repos/homeshick/completions/homeshick-completion.bash"

### Bash auto completion
source /etc/profile.d/bash_completion.sh

### Gradle auto completion
#source $HOME/bash_completion.d/gradle-completion.bash

### Java Environment Variables
export JAVA_HOME="/usr/lib/jvm/java-11-openjdk-11.0.14.1.1-1.el7_9.x86_64"
export JAVA_BIN="$JAVA_HOME/bin"
export EXTERN_HOME="$HOME/repos/web-umc_dependencies"
export GDS_HOME="$HOME/repos/web-umc_dependencies/GIAC/x86_64-Linux-RHEL7"

### History format
HISTTIMEFORMAT=' %a %B %d | [%T] | '

export PATH=~/.npm-global/bin:$PATH

#export M2_HOME=/opt/maven
#export MAVEN_HOME=/opt/maven
#export PATH=$M2_HOME/bin:$PATH



##########################
# ALIASES
##########################
alias resource='source ~/.bashrc'
alias userlist="eval getent passwd {$(awk '/^UID_MIN/ {print $2}' /etc/login.defs)..$(awk '/^UID_MAX/ {print $2}' /etc/login.defs)} | cut -d: -f1"
alias chownjs="sudo chown jstoup:jstoup *"
alias less='less -R' #need this so colorized text displays correctly

### grepsmall returns just the salient value pulled from grep, not the entire file
function _grepsmall {
    grep -E -o ".{0,30}$1.{0,30}" * | sed 's/:/:  /'
}
alias grepsmall='_grepsmall'

function _create_git_branch_and_track {
    BRANCH_NAME=$1

    git checkout -b $BRANCH_NAME
    git push --set-upstream origin $BRANCH_NAME
}
alias gitcatlb='_create_git_branch_and_track'

web_umc_version="web-umc-server-6.6.0.0"
alias start-webumc="cd $HOME/repos/web-umc/server/build/distributions/${web_umc_version}; ./start-web-umc-server.sh"
alias start-webumc-debug="cd $HOME/repos/web-umc/server/build/distributions/${web_umc_version}; ./start-web-umc-server.sh -d"
alias print-webumc-ip="echo '/home/jstoup/repos/web-umc/server/build/distributions/${web_umc_version}/install/config/web-umc-server-config.yml'; grep mssvEndpoint /home/jstoup/repos/web-umc/server/build/distributions/${web_umc_version}/install/config/web-umc-server-config.yml"
alias start-mssv="cd $HOME/repos/mssv/dist; chmod a+x start.sh; ./start.sh"
alias build-webumc-clean="cd $HOME/repos/web-umc; ./gradlew clean build -P simple --settings-file=simple-settings.gradle; cd $HOME/repos/web-umc/server/build/distributions/; tar -xzf *.tgz; cd -"
alias build-webumc="cd $HOME/repos/web-umc; ./gradlew build -P simple --settings-file=simple-settings.gradle; cd $HOME/repos/web-umc/server/build/distributions/; tar -xzf *.tgz; cd -"
alias build-mssv="cd ${HOME}/repos/mssv; ./mvnw clean install -Pdist -DskipTests=true"

function _start_atd() {
    cd ${HOME}/repos/mssv/clients/air-test-driver/target
    java -jar airtestdriver-all.jar $1
}
alias start-atd='_start_atd'

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
            test "$branch" != main || branch=' '            
        else
            # Detached HEAD.  (branch=HEAD is a faster alternative.)
            branch="(`git describe --all --contains --abbrev=4 HEAD 2> /dev/null ||
            		          echo HEAD`)"
        fi
        echo -n '\[\e[0;37;'"$ansi"';1m\]'"$branch"'\[\e[0m\] '
    fi
}

function _prompt_command() {
    PS1="`_git_prompt`(\[\033[0;31m\]\u - \t\[\033[0;30m\])\n  [\[\033[0;34m\]\w\[\033[0;30m\]]\$ " 
}

PROMPT_COMMAND=_prompt_command


#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"



#LS_COLORS=$LS_COLORS:'di=0;94:' ; export LS_COLORS
export N_PREFIX="$HOME/.n"
export PATH="$HOME/.npm/bin:$PATH"

source /opt/rh/devtoolset-10/enable
export CMAKE_C_COMPILER=/opt/rh/devtoolset-10/root/usr/bin/gcc
export CMAKE_CXX_COMPILER=/opt/rh/devtoolset-10/root/usr/bin/g++

export AFMSTT_VERSION=latest






export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
