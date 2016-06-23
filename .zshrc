autoload -Uz colors && colors
autoload -Uz compinit && compinit
autoload -Uz promptinit && promptinit
autoload -Uz vcs_info

zstyle ':completion:*' completer         _complete _ignored _correct _approximate
zstyle ':completion:*' matcher-list      '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}'
zstyle :compinstall    filename          '/home/mitchell/.zshrc'
zstyle ':vcs_info:*'   check-for-changes true
zstyle ':vcs_info:*'   check-for-stages  true
zstyle ':vcs_info:*'   stagedstr         '%B%F{green}•%f%b'
zstyle ':vcs_info:*'   unstagedstr       '%B%F{yellow}•%f%b'
zstyle ':vcs_info:*'   actionformats     ' %F{cyan}%b%c%u%F{yellow}|%F{red}%a%f'
zstyle ':vcs_info:*'   formats           ' %F{cyan}%b%c%u%f'
zstyle ':vcs_info:*'   enable git

precmd () { vcs_info }

export PATH=~/.local/bin:$PATH

alias g='git'
alias ls='ls --color'
alias vi='nvim'
alias vim='nvim'

# todo.txt
export TODOTXT_DEFAULT_ACTION=ls
export TODOTXT_SORT_COMMAND='env LC_COLLATE=C sort -k 2,2 -k 1,1n'
alias t='todo.sh'

setopt appendhistory
setopt autocd
setopt notify
setopt prompt_subst
unsetopt beep
bindkey -e

zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

# Pull git_info into a variable so it's recomputed, not cached.
local git_info='${vcs_info_msg_0_}'
PROMPT="[%1(j.%F{magenta}%j%f .)%B%F{yellow}%~%f%b${git_info}] "
RPROMPT="%F{blue}%n%f•%F{green}%M%f%0(?..•%F{red}%?)"

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /home/mitchell/.config/zsh/plugins/zsh-history-substring-search.zsh
