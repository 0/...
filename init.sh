#!/bin/bash

set -e

if [[ -z "$1" ]]; then
	echo "usage: $0 <path>"
	exit 1
fi

DOTFILES="$1"

#ln -sT "$DOTFILES"/Xdefaults ~/.Xdefaults
#ln -sT "$DOTFILES"/abcde.conf ~/.abcde.conf
#ln -sT "$DOTFILES"/bin ~/bin
#ln -sT "$DOTFILES"/clisprc ~/.clisprc
#ln -sT "$DOTFILES"/ghc ~/.ghc
#ln -sT "$DOTFILES"/gitconfig ~/.gitconfig
#ln -sT "$DOTFILES"/gitignore ~/.gitignore
#ln -sT "$DOTFILES"/inputrc ~/.inputrc
#ln -sT "$DOTFILES"/mspdebug ~/.mspdebug
#ln -sT "$DOTFILES"/nethackrc ~/.nethackrc
#ln -sT "$DOTFILES"/oh-my-zsh ~/.oh-my-zsh
#ln -sT "$DOTFILES"/racketrc ~/.racketrc
#ln -sT "$DOTFILES"/sbclrc ~/.sbclrc
#ln -sT "$DOTFILES"/screenrc ~/.screenrc
#ln -sT "$DOTFILES"/tmux.conf ~/.tmux.conf
#ln -sT "$DOTFILES"/vim ~/.vim
#ln -sT "$DOTFILES"/xinitrc ~/.xinitrc
#ln -sT "$DOTFILES"/xmobarrc ~/.xmobarrc
#ln -sT "$DOTFILES"/xmonad ~/.xmonad
#ln -sT "$DOTFILES"/xscreensaver ~/.xscreensaver
#ln -sT "$DOTFILES"/zshrc ~/.zshrc

#mkdir -p ~/.config
#ln -sT ../"$DOTFILES"/config/htop ~/.config/htop
#ln -sT ../"$DOTFILES"/config/matplotlib ~/.config/matplotlib

#mkdir -p ~/.julia
#ln -sT "$DOTFILES"/julia/config ~/.julia/config
