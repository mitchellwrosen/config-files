export TERM="rxvt-unicode-256color"
export LANG="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export EDITOR="vim"

export GHCROOT=/usr/

export GOROOT=/usr/lib/go
export GOPATH=$HOME/projects/go
export PATH=$PATH:$GOROOT/bin

#export PATH=$PATH:~/Downloads/google_appengine

# Wrap gcc with colorgcc
export PATH=$PATH:/usr/lib/colorgcc/bin

export PATH=$PATH:~/.cabal/bin
export PATH=$PATH:~/.gem/ruby/2.0.0/bin

export PATH=$PATH:~/bin

# Tell ccache to only use compilers here. Remember to TELL COLORGCC TO CALL
# CCACHE INSTEAD OF THE REAL COMPILER by modifying /etc/colorgcc/colorgcc
export CCACHE_PATH=/usr/bin

autoload -U colors && colors
PS1="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m%{$fg[green]%}%~ %{$fg[yellow]%}%# %{$reset_color%}%"

# Common alias
alias ls='ls -F --color=auto'
alias ll='ls -lh'
alias la='ls -lha'
alias grep='grep --color=auto'
alias g='git '

# dev_appserver alias
#alias appserver='python2 /opt/google-appengine-go/dev_appserver.py'

setopt CORRECT # Spell check commands
#setopt RM_STAR_WAIT # 10 second wait if you do rm *
setopt NO_CASE_GLOB # Case insensitive globbing
setopt NUMERIC_GLOB_SORT
setopt EXTENDED_GLOB

#bindkey '\e[1~' beginning-of-line
#bindkey '\e[4~' end-of-line
bindkey -s '\eu' '^Ucd ..; ls^M'

# History stuff
HISTFILE=~/.zsh_hist
SAVEHIST=10000
HISTSIZE=10000
setopt APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
setopt EXTENDED_HISTORY
setopt HIST_SAVE_NO_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS
