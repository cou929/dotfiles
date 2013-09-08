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

## PATH
export PATH=$PATH:/usr/local/bin:${HOME}/projects/utils:${HOME}/share/pyenv/bin:${HOME}/perl5/bin:${HOME}/Dropbox/resource/dotfiles/git_hook_scripts

## charcter encoding
export LANG=ja_JP.UTF-8

## rvm
[[ -s "/Users/${ME}/.rvm/scripts/rvm" ]] && source "/Users/${ME}/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

## python env
export WORKON_HOME=$HOME/.virtualenvs
. virtualenvwrapper.sh

mkvenv () {
    base_python=`which python$1` 
    mkvirtualenv --distribute --python=$base_python $2
}

## ssh and new screen tab
function sssh {
    screen -t $1 ssh $1
}

## zsh-syntax-highlighting
source ~/src/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

## perlbrew
export PERLBREW_PERL=perl-5.16.2
source ~/perl5/perlbrew/etc/bashrc

## nodebrew
export PATH=$HOME/.nodebrew/current/bin:$PATH

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

## for fout perl
export FOUT_HOME=$HOME/mnt/fout/
export FOUTUI_HOME=$HOME/mnt/fout_ui/
export PERL5LIB=${FOUT_HOME}lib:${FOUT_HOME}extlib/lib/perl5/x86_64-linux/:${FOUT_HOME}extlib/lib/perl5/:{FOUT_HOME}t/lib/:${FOUTUI_HOME}lib:${FOUTUI_HOME}extlib/lib/perl5/x86_64-linux/:${FOUTUI_HOME}extlib/lib/perl5/:~/perl5/lib/perl5/
