#!/bin/bash

set -e

if [[ -z "$1" ]]; then
	echo "usage: $0 <path>"
	exit 1
fi

DOTFILES="$1"

if [[ "${DOTFILES:0:1}" != '/' ]]; then
	echo 'Absolute path required'
	exit 1
fi

#ln -sT "$DOTFILES"/Xdefaults ~/.Xdefaults
#ln -sT "$DOTFILES"/bin ~/bin
#ln -sT "$DOTFILES"/gitconfig ~/.gitconfig
#ln -sT "$DOTFILES"/gitignore ~/.gitignore
#ln -sT "$DOTFILES"/inputrc ~/.inputrc
#ln -sT "$DOTFILES"/mspdebug ~/.mspdebug
#ln -sT "$DOTFILES"/nethackrc ~/.nethackrc
#ln -sT "$DOTFILES"/racketrc ~/.racketrc
#ln -sT "$DOTFILES"/sbclrc ~/.sbclrc
#ln -sT "$DOTFILES"/screenrc ~/.screenrc
#ln -sT "$DOTFILES"/tmux.conf ~/.tmux.conf
#ln -sT "$DOTFILES"/vim ~/.vim
#ln -sT "$DOTFILES"/xmonad ~/.xmonad
#ln -sT "$DOTFILES"/xscreensaver ~/.xscreensaver
#ln -sT "$DOTFILES"/zsh ~/.zsh
#ln -sT "$DOTFILES"/zsh/zprofile ~/.zprofile
#ln -sT "$DOTFILES"/zsh/zshrc ~/.zshrc

#cp -n "$DOTFILES"/gitconfig.local.example ~/.gitconfig.local
#cp -n "$DOTFILES"/zsh/zprofile.local.example ~/.zprofile.local
#cp -n "$DOTFILES"/zsh/zshrc.local.example ~/.zshrc.local

#mkdir -p ~/.ssh
#cp -n "$DOTFILES"/ssh/config.example ~/.ssh/config

#mkdir -p ~/.config
#ln -sT "$DOTFILES"/config/matplotlib ~/.config/matplotlib

#mkdir -p ~/.julia
#ln -sT "$DOTFILES"/julia/config ~/.julia/config
