function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('`basename $VIRTUAL_ENV`') '
}

function box_name {
    [ -f ~/.box-name ] && cat ~/.box-name || hostname -s
}

local current_dir='${PWD/#$HOME/~}'
local git_info='$(git_prompt_info)'

PROMPT="${PIPESTATUS[*]}[%{$terminfo[bold]$FG[226]%}${current_dir}%{$reset_color%}${git_info}] • "
RPROMPT="[%{$terminfo[bold]$FG[040]%}%n%{$reset_color%} %{$FG[239]%}♪ %{$terminfo[bold]$FG[033]%}$(box_name)%{$reset_color%}]"

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$FG[239]%}↟%{$reset_color%} %{$fg[255]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$FG[202]%}✘"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$FG[040]%}✔"
