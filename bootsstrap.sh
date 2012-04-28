#! /bin/sh

USER_NAME=kosei
DROPBOX_PATH=/Users/${USER_NAME}/Dropbox
DOT_PATH=${DROPBOX_PATH}/resource/dotfiles

ln -s ${DROPBOX_PATH}/projects ~/projects
ln -s ${DOT_PATH}/.emacs ~/.emacs
ln -s ${DOT_PATH}/.zshrc ~/.zshrc
ln -s ${DOT_PATH}/.screenrc ~/.screenrc
ln -s ${DOT_PATH}/.emacs.d ~/.emacs.d
ln -s ${DOT_PATH}/.hgrc ~/.hgrc
ln -s ${DROPBOX_PATH}/memo ~/memo
ln -s ${DOT_PATH}/.gitconfig ~/.gitconfig

# prepare pythonenv
mkdir -p ~/share
${DROPBOX_PATH}/projects/utils/virtualenv.py ~/share/pyenv
