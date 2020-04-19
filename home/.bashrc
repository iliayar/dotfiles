[[ $- != *i* ]] && return

LANG=en_US.UTF-8
TERM=xterm-256color

alias svim="/usr/bin/vim"
alias vim="/usr/bin/vim -Nu ~/.myvim/.vimrc" 

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

PS1="\[\e[34m\]\w\[\e[m\] λ "

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
