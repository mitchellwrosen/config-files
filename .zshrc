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

alias g=git
alias ls='ls --color'
alias vi=nvim
alias vim=nvim
alias gvi='NVIM_GTK_NO_HEADERBAR=1 nvim-gtk'
alias gvim='NVIM_GTK_NO_HEADERBAR=1 nvim-gtk'

EDITOR=vim
PATH=~/.ghcup/bin:~/.local/bin:~/.npm/node_modules/.bin:$PATH
PROMPT='%(1j.%j .)%(?..%F{red}%?%f )%~ '
RPROMPT='${vcs_info_msg_0_}'

function cd {
  builtin cd "$@"
  ls
}

# Nub the path
typeset -U path

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'

if [ -e /home/mitchell/.nix-profile/etc/profile.d/nix.sh ]; then
  . /home/mitchell/.nix-profile/etc/profile.d/nix.sh;
fi

# [base16-shell]
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

# [zsh-autosuggestions]
# pacman -S zsh-autosuggestions
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

export SENTENAI_MONOREPO=cabal
