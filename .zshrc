## zsh confs
## via http://bloghackers.net/~naoya/webdb40/

# auto complete
autoload -U compinit
compinit

# command history
HISTFILE=$HOME/.zsh-history
HISTSIZE=10000000
SAVEHIST=10000000
setopt hist_ignore_dups
setopt share_history

# cd settings
setopt auto_cd
setopt auto_pushd

# command correction
setopt correct

# pack list candidates
setopt list_packed

# disable beeps
setopt nobeep
setopt nolistbeep

limit coredumpsize 102400
unsetopt promptcr
setopt prompt_subst
setopt long_list_jobs
setopt list_types
setopt auto_resume
setopt auto_list
setopt autopushd
setopt pushd_ignore_dups
setopt extended_glob
setopt auto_menu
setopt extended_history
setopt equals
setopt magic_equal_subst
setopt hist_verify
setopt numeric_glob_sort
setopt print_eight_bit
zstyle ':completion:*:default' menu select=1
#zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
#zstyle ':completion:*:default' list-colors $LSCOLORS
zstyle ':completion:*' list-colors 'di=36' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
setopt auto_param_keys
setopt auto_param_slash

# prompt
PROMPT="%m:%n%% "
RPROMPT="[%/]"

# emacs-like keybind
bindkey -e

# keybind for history search
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

## alias
alias fcd='source ~/bin/scd.sh'
alias cemacs='open -a Emacs'
alias firefox='open -a Firefox'
alias safari='open -a Safari'
alias preview='open -a Preview'
alias acroread='open -a /Applications/Adobe\ Reader\ 9/Adobe\ Reader.app'
alias textedit='open -a TextEdit'
alias la="ls -a"
alias ll="ls -l"
alias du="du -h"
alias df="df -h"

export PATH=$PATH:/opt/local/bin:/usr/local/bin:/usr/local/texlive/2008/bin/universal-darwin:/Users/kosei/projects/utils:/opt/local/lib

# http_proxy="http://proxy.cc.chuo-u.ac.jp:8080/"
# HTTP_PROXY="http://proxy.cc.chuo-u.ac.jp:8080/"
# https_proxy="proxy.cc.chuo-u.ac.jp:8080"
# HTTPS_PROXY="proxy.cc.chuo-u.ac.jp:8080"
# FTP_PROXY="proxy.cc.chuo-u.ac.jp:8080"
# ftp_proxy="proxy.cc.chuo-u.ac.jp:8080"
# ALL_PROXY="proxy.cc.chuo-u.ac.jp:8080"
# gopher_proxy=http://proxy.cc.chuo-u.ac.jp:8080/
# wais_proxy=http://proxy.cc.chuo-u.ac.jp:8080/
# no_proxy="localhost,my.domain"
# export gopher_proxy wais_proxy no_proxy
# export http_proxy HTTP_PROXY https_proxy HTTPS_PROXY FTP_PROXY ftp_proxy ALL_PROXY

# for xapian
PERL5LIB=/Library/Perl/5.8.8/darwin-thread-multi-2level/auto/Search/Xapian/.libs
export PERL5LIB

# for Qt3
QTDIR=/opt/local/lib/qt3
export QTDIR

# for CGAL
export CGAL_MAKEFILE="/Users/kosei/projects/CGAL-3.3.1/make/makefile_i386_Darwin-9.6_g++-4.0.1"

# for terminal color
export LSCOLORS=gxfxcxdxbxegedabagacad
alias ls="ls -G -w -F"

# Amazon Web Service keys
export AWS_ACCESS_KEY=AKIAIMEJGSRXGFILQI6A
export AWS_SECRET_KEY=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

# access git over proxy
export GIT_PROXY_COMMAND=/Users/kosei/.ssh/proxy_cmd_for_github

# add my libraries to gcc include path
export CPATH=/Users/kosei/projects/utils

# add opt dirs to loadpath
export LIBRARY_PATH=$LIBRARY_PATH:/opt/local/lib
export CPATH=$CPATH:/opt/local/include

# pythonpath
export PYTHONPATH=/opt/local/lib/python2.5/site-packages

# localle and language
export LANG=en_US.URF-8
