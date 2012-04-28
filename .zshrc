## zsh confs

# auto complete
autoload -U compinit
compinit

# command history
HISTFILE=$HOME/Dropbox/resource/zsh_history/.zsh-history
HISTSIZE=10000000
SAVEHIST=10000000
setopt hist_ignore_dups
setopt share_history

setopt auto_cd # cd by dir name
setopt autopushd # auto pushd
# setopt auto_pushd 
setopt correct # command correction
setopt list_packed # pack list candidates
setopt nobeep # disable beep
setopt nolistbeep # disable beep
setopt noautoremoveslash # disable '/' auto remove
setopt complete_aliases # aliased ls needs if file/dir completions work

limit coredumpsize 102400
unsetopt promptcr
setopt prompt_subst
setopt long_list_jobs
setopt list_types
setopt auto_resume
setopt auto_list
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
PROMPT="[%T %n@%m %c]%% "
RPROMPT="[%/]"

# emacs-like keybind
bindkey -e

# keybind for history search
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# for terminal color
export LSCOLORS=gxfxcxdxbxegedabagacad
alias ls="ls -G -w -F"

## alias
alias cemacs="open -n /Applications/Emacs.app"
alias firefox='open -a Firefox'
alias safari='open -a Safari'
alias preview='open -a Preview'
alias acroread='open -a /Applications/Adobe\ Reader\ 9/Adobe\ Reader.app'
alias textedit='open -a TextEdit'
alias la="ls -a"
alias ll="ls -l"

## git aliases
alias 'g'='git'

## virtualenv alias
alias 'py'='source ~/share/pyenv/bin/activate'

## you may write feature experiments or machine specific settings to .zshrc.mine
[ -f ~/.zshrc.mine ] && source ~/.zshrc.mine

## PATH
MAC_HOME=/Users/kosei
export PATH=$PATH:/usr/local/bin:${MAC_HOME}/projects/utils:${MAC_HOME}/share/pyenv/bin

## add my libraries to gcc include path
export CPATH=/Users/kosei/projects/utils

## charcter encoding
export LANG=ja_JP.UTF-8

## titanium
alias titanium='/Library/Application\ Support/Titanium/mobilesdk/osx/1.6.1/titanium.py'

## nvm
## http://d.hatena.ne.jp/mollifier/20110221/p1
if [[ -f ~/.nvm/nvm.sh ]]; then
  source ~/.nvm/nvm.sh

  if which nvm >/dev/null 2>&1 ;then
    _nodejs_use_version="v0.4.9"
    if nvm ls | grep -F -e "${_nodejs_use_version}" >/dev/null 2>&1 ;then
      nvm use "${_nodejs_use_version}" >/dev/null
    fi
    unset _nodejs_use_version
  fi
fi

## gisty by swdyh
export GISTY_DIR="$HOME/projects/gists"

## rvm
[[ -s "/Users/kosei/.rvm/scripts/rvm" ]] && source "/Users/kosei/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
