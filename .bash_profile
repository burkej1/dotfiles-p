# Github tab completion
source ~/.git-completion.bash

export CLICOLOR=1

export LSCOLORS=GxFxCxDxBxegedabagaced

# Aliases
alias less="less -nS"  # Open less with no line wrapping or numbers by default

# Prompt
export PS1="__ __ __\n\w \n| => "
export PS2="| => "

# Setting vim as the default editor (for use with Ctrl-x Ctrl-e)
export VISUAL=vim
export EDITOR="$VISUAL"

# Source any local aliases/path settings
if [ -f ~/.bash_profile_local ]; then
    source ~/.bash_profile_local
fi

