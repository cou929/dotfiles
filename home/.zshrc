## zsh confs

# include site functions
if which brew > /dev/null; then
    fpath=($(brew --prefix)/share/zsh/site-functions $fpath)
else
    fpath=(~/.zsh/completion $fpath)
fi

# auto complete
autoload -U compinit
compinit -u

# command history
HISTFILE=$HOME/Dropbox/resource/zsh_history/.zsh-history
HISTSIZE=10000000
SAVEHIST=10000000
setopt hist_ignore_dups
setopt share_history

setopt auto_cd # cd by dir name
setopt autopushd # auto pushd
setopt correct # command correction
setopt list_packed # pack list candidates
setopt nobeep # disable beep
setopt nolistbeep # disable beep
setopt noautoremoveslash # disable '/' auto remove
setopt complete_aliases # aliased ls needs if file/dir completions work
setopt no_complete_aliases # git completions for 'g' alias

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
zstyle ':completion:*' list-colors 'di=36' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
setopt auto_param_keys
setopt auto_param_slash

# prompt
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats ' (%b)'
zstyle ':vcs_info:*' actionformats ' (%b|%a)'
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}

PROMPT="[%T %n@%m %c%1(v|%F{green}%1v%f|)]%% "

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
alias ll="ls -l"
alias g='git'
alias s='git st'
alias m='git co master'
alias d='git diff'

## PATH
export PATH=/usr/local/bin:$PATH:${HOME}/projects/utils

## charcter encoding
export LANG=ja_JP.UTF-8

## static httpd
function static_httpd {
  if which plackup > /dev/null; then
    plackup -MPlack::App::Directory -e 'Plack::App::Directory->new(root => ".")->to_app'
  elif which python > /dev/null; then
    if python -V 2>&1 | grep -qm1 'Python 3\.'; then
      python -m http.server 5000
    else
      python -m SimpleHTTPServer 5000
    fi
  elif which node > /dev/null; then
    node -e "var c=require('connect'), d=process.env.PWD; c().use(c.logger()).use(c.static(d)).use(c.directory(d)).listen(5000);"
  elif which ruby > /dev/null; then
    ruby -rwebrick -e 'WEBrick::HTTPServer.new(:Port => 5000, :DocumentRoot => ".").start'
  elif which php > /dev/null && php -v | grep -qm1 'PHP 5\.[45]\.'; then
    php -S 0.0.0.0:5000
  elif which erl > /dev/null; then
    erl -eval 'inets:start(), inets:start(httpd, [{server_name, "httpd"}, {server_root, "."}, {document_root, "."}, {port, 5000}])'
  fi
}

# ll envs
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
if which plenv > /dev/null; then eval "$(plenv init -)"; fi
source $(brew --prefix nvm)/nvm.sh
source /usr/local/bin/virtualenvwrapper.sh

# eclipse
export PATH=$PATH:/Applications/eclipse/android/platform-tools

# added by travis gem
[ -f /Users/kosei/.travis/travis.sh ] && source /Users/kosei/.travis/travis.sh

# google cloud sdk
source "$HOME/google-cloud-sdk/path.zsh.inc"
source "$HOME/google-cloud-sdk/completion.zsh.inc"

# boot2docker
export DOCKER_TLS_VERIFY=1
export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=$HOME/.boot2docker/certs/boot2docker-vm

# jdk for android studio
export STUDIO_JDK=/Library/Java/JavaVirtualMachines/jdk1.7.0_71.jdk

# [test] check google js style guide update
curl -s http://google-styleguide.googlecode.com/svn/trunk/javascriptguide.xml -o /var/tmp/gjssg.1 && diff -u /var/tmp/gjssg /var/tmp/gjssg.1 > /var/tmp/diff || cat /var/tmp/diff | echo "[DIFF] Google JavaScript Style Guide" ; mv /var/tmp/gjssg.1 /var/tmp/gjssg
export PATH="$HOME/.embulk/bin:$PATH"

# golang
export GOPATH=$HOME/go1.4
export GOROOT=/usr/local/bin/go
export PATH=$GOPATH/bin:$GOROOT/bin:$PATH
