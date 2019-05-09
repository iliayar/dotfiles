[[ $- != *i* ]] && return

LANG=en_US.UTF-8
TERM=xterm-256color

PATH+=:$HOME/bin


if [ -x /usr/bin/dircolors ]; then
    # Colorful ls
    alias ls='ls --color=auto'
    # Colorful grep
    alias grep='grep --color=auto'
fi

# Configuration

shopt -s expand_aliases

# Disable logging repeated commands
export HISTCONTROL=ignoredups
# Append to history
shopt -s histappend
# export PROMPT_COMMAND='history -a;history -c;history -r'
# Complete after *comand*
complete -cf sudo
complete -cf man

PS1="\e[0;31m\u"
PS1+="\e[0;36m@"
PS1+="\e[1;31m\h"
PS1+="\e[0;36m: "
PS1+="\e[0;36m\w "
PS1+="\e[0;36mλ"
PS1+="\e[0m "
