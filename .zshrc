HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_BEEP

# prompt_subst - required for git info in prompt
setopt appendhistory autocd notify prompt_subst
unsetopt beep
bindkey -v
bindkey '^R' history-incremental-search-backward

# The following lines were added by compinstall
zstyle :compinstall filename '/home/mitchell/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

autoload -Uz vcs_info
# Only detect git repos
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*' formats "%b"
zstyle ':vcs_info:git*' actionformats "%b|%a"

# Executed before each prompt.
precmd() {
  vcs_info
}

alias cat=bat
alias g=git
alias ls=exa
alias vi=nvim
alias vim=nvim
alias gvi='NVIM_GTK_NO_HEADERBAR=1 nvim-gtk'
alias gvim='NVIM_GTK_NO_HEADERBAR=1 nvim-gtk'
alias tree='exa -T'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

export EDITOR=vim
export PATH=~/.ghcup/bin:~/.local/bin:~/golang/bin:~/.npm/node_modules/.bin:$PATH
export PROMPT='%(1j.%j .)%(?..%F{red}%?%f )%~ '
export RPROMPT='${vcs_info_msg_0_}'

function cd {
  builtin cd "$@"
  ls
}

# Nub the path
typeset -U path

# [base16-shell]
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

# [zsh-autosuggestions]
# pacman -S zsh-autosuggestions
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# fzf
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh

# direnv
eval "$(direnv hook zsh)"
export DIRENV_LOG_FORMAT= # shut direnv up when I cd around
