#! /bin/sh

DROPBOX_PATH=${HOME}/Dropbox
DOT_PATH=${DROPBOX_PATH}/resource/dotfiles

ln -s ${DROPBOX_PATH}/projects ~/projects
ln -s ${DOT_PATH}/.emacs ~/.emacs
ln -s ${DOT_PATH}/.zshrc ~/.zshrc
ln -s ${DOT_PATH}/.screenrc ~/.screenrc
ln -s ${DOT_PATH}/.emacs.d ~/.emacs.d
ln -s ${DOT_PATH}/.hgrc ~/.hgrc
ln -s ${DROPBOX_PATH}/memo ~/memo
ln -s ${DROPBOX_PATH}/src ~/src
ln -s ${DROPBOX_PATH}/Apps/note_updeter/ChangeLog.txt ~/memo/ChangeLog.txt
ln -s ${DOT_PATH}/.gitconfig ~/.gitconfig
