#!/bin/bash

# Symlinks dot files, sets up folders and clones plugins
# Requires:
#   - bash
#   - git
#   - wget
#   - curl
#   - tmux
#   - vim

# Make sure dotfiles folder exists in home
if [ ! -d ~/dotfiles ]; then
    exit 1
fi


# # # Bash Setup # # #
ln -s ~/dotfiles/.git-completion.bash ~/
ln -s ~/dotfiles/.bash_profile ~/
source ~/.bash_profile


# # # Vim Setup # # # 
# Symlink .vimrc
ln -s ~/dotfiles/.vimrc ~/

# Install Pathogen
mkdir -p ~/.vim/autoload ~/.vim/bundle && \
    curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

# Cloning Plugins
cd ~/.vim/bundle
git clone https://github.com/liuchengxu/space-vim-dark
git clone https://github.com/vim-airline/vim-airline
git clone https://github.com/justinmk/vim-sneak
git clone https://github.com/easymotion/vim-easymotion
git clone https://github.com/w0rp/ale
git clone https://github.com/Konfekt/FastFold
git clone https://github.com/scrooloose/nerdtree
git clone https://github.com/ervandew/supertab
git clone https://github.com/tpope/vim-sensible
git clone https://github.com/tpope/vim-commentary
git clone https://github.com/kshenoy/vim-signature
cd


# # # TMUX Setup # # # 
# Symlink .tmux.conf
ln -s ~/dotfiles/.tmux.conf ~/

# Install TMUX plugin manager (tpm)
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Clone themepacks
git clone https://github.com/jimeh/tmux-themepack.git ~/.tmux-themepack

# # Reload TMUX environment (better to handle manually I think)
# tmux source ~/.tmux.conf

