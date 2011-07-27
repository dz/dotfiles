#!/usr/bin/bash

# Inspired greatly by Armin Ronacher's bashrc
# https://github.com/mitsuhiko/dotfiles/blob/master/bash/bashrc

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

DEFAULT_COLOR="[00;1m"
GRAY_COLOR="[37;1m"
PINK_COLOR="[35;1m"
GREEN_COLOR="[32;1m"
CYAN_COLOR="[36;1m"
ORANGE_COLOR="[33;1m"
RED_COLOR="[31;1m"
if [ `id -u` == '0' ]; then
  USER_COLOR=$RED_COLOR
else
  USER_COLOR=$ORANGE_COLOR
fi

VCPROMPT_EXECUTABLE=/usr/bin/vcprompt
SSHUTTLE_EXECUTABLE=/usr/bin/vcprompt

function vcprompt() {
  $VCPROMPT_EXECUTABLE -f $' (\033[31;1m%[unknown]b\033[32;1m%m%u\033[37;1m)'
}

function lastcommandfailed() {
  code=$?
  if [ $code != 0 ]; then
    echo -n $'\033[37;1m exited \033[31;1m'
    echo -n $code
    echo -n $'\033[37;1m'
  fi
}

function backgroundjobs() {
  jobs|python -c 'if 1:
    import sys
    items = ["\033[36;1m%s\033[37;1m" % x.split()[2]
             for x in sys.stdin.read().splitlines()]
    if items:
      if len(items) > 2:
        string = "%s, and %s" % (", ".join(items[:-1]), items[-1])
      else:
        string = ", ".join(items)
      print "\033[37;1m running %s" % string
  '
}

function activevirtualenv() {
  if [ -n "$VIRTUAL_ENV" ]; then
      echo -n $'\033[00;1m\033[36;1m('
      echo -n "${VIRTUAL_ENV#$WORKON_HOME/}) "
  fi
}

# export BASEPROMPT='\e${USER_COLOR}\u \
# \e${GRAY_COLOR}at \e${ORANGE_COLOR}\h \
# \e${GRAY_COLOR}$(vcprompt)\
# \e${GRAY_COLOR}in \e${GREEN_COLOR}\w\
# `backgroundjobs`\
# \e${DEFAULT_COLOR}'
export BASEPROMPT='\n\e${USER_COLOR}\u\
`activevirtualenv` \
\e${GRAY_COLOR}[\e${GREEN_COLOR}\w\
\e${GRAY_COLOR}]$(vcprompt)\
`backgroundjobs`\
\e${DEFAULT_COLOR}'

export BASEPROMPT='`activevirtualenv`\e${GRAY_COLOR}\e${GREEN_COLOR}\w\e${GRAY_COLOR}$(vcprompt)\e${GRAY_COLOR} \e${USER_COLOR}\u\e${DEFAULT_COLOR}'

export PROMPT="${BASEPROMPT}$ "
export PS1=$PROMPT

# export TERM=xterm-color
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'

export CLICOLOR=1
if [ `uname` == "Darwin" ]; then
  export LSCOLORS=ExGxFxDxCxHxHxCbCeEbEb
  export LC_CTYPE=en_US.utf-8
else
  alias ls='ls --color=auto -F'
fi
export IGNOREEOF=1
export LESS=FRSX

export LIBTOOLIZE=glibtoolize

# python
export PYTHONDONTWRITEBYTECODE=1
export PYTHONSTARTUP="$HOME/.pythonrc.py"

# virtualenvwrapper and pip
if [ `id -u` != '0' ]; then
  export VIRTUALENV_USE_DISTRIBUTE=1
  export WORKON_HOME=$HOME/.virtualenvs
  source /usr/local/bin/virtualenvwrapper.sh
  export PIP_VIRTUALENV_BASE=$WORKON_HOME
  export PIP_REQUIRE_VIRTUALENV=true
  export PIP_RESPECT_VIRTUALENV=true
  #export PIP_USE_MIRRORS=true
fi

# Enable bash history
export HISTCONTROL=erasedups
TSIZE=50000
shopt -s histappend

export FALLBACK_DYLD_LIBRARY_PATH=/usr/local/lib

# These set up/down to do the history searching
bind '"\e[A"':history-search-backward
bind '"\e[B"':history-search-forward

alias c="clear"
alias l="ls --color -F"
alias less="less -R"
alias gitlg="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen%cn%Creset %Cblue%cr%Creset' --abbrev-commit --date=relative"
export GREP_OPTIONS='--color=auto'

#emacs aliases
alias emacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n -c -a /Applications/Emacs.app/Contents/MacOS/Emacs"
alias emacst="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t -a /Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias et="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t -a /Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias em="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n -a /Applications/Emacs.app/Contents/MacOS/Emacs"

#sublime text aliases
alias slt='open -a "Sublime Text 2"'

#vim aliases
alias vim="/Applications/MacVim.app/Contents/MacOS/Vim"

#things for bash history to ignore
export HISTIGNORE="&:ls:[bf]g:exit:clear:c"

source ~/.git-completion.sh

export PATH=/opt/local/lib/postgresql84/bin:/opt/local/Library/Frameworks/Python.framework/Versions/2.6/bin/:/opt/local/bin:/opt/local/sbin:/Users/dzhou/.gem/ruby/1.8:$PATH

#virtualenv stuff
export WORKON_HOME=~/.virtualenvs
source /opt/local/Library/Frameworks/Python.framework/Versions/2.6/bin/virtualenvwrapper.sh
