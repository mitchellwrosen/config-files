export TERM="rxvt-unicode-256color"
export LANG="en_US.UTF-8"
export EDITOR="vim"

export GOROOT=/usr/lib/go
export GOPATH=$HOME/projects/go
export PATH=$PATH:$GOROOT/bin

export PATH=$PATH:~/scripts
export PATH=$PATH:~/Downloads/google_appengine

export PATH=$PATH:~/.gem/ruby/1.9.1/bin

# Wrap gcc with colorgcc
export PATH=$PATH:/usr/lib/colorgcc/bin

# Tell ccache to only use compilers here. Remember to TELL COLORGCC TO CALL
# CCACHE INSTEAD OF THE REAL COMPILER by modifying /etc/colorgcc/colorgcc
export CCACHE_PATH=/usr/bin

autoload -U colors && colors
PS1="%{$fg[yellow]%}[%T] %{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m%{$fg[green]%}%~ %{$fg[yellow]%}%# %{$reset_color%}%"
#PS1='\[\e[01;32m\]\u\[\e[0m\]:\[\e[01;35m\]\[\e[0m\]:\[\e[01;34m\]\w\[\e[0m\]\$ '

# Common alias
alias ls='ls -F --color=auto'
alias ll='ls -lh'
alias grep='grep --color=auto'
