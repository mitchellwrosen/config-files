function fish_right_prompt
  set -l last_status $status
  set -l prompt_status
  if test $last_status -ne 0
    set prompt_status (set_color $fish_color_status) "$last_status" "$normal"
  end


  if not set -q __fish_prompt_hostname
    set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
  end

  echo -n -s $prompt_status (set_color normal) " [" (set_color green) "$USER" (set_color normal) @ (set_color cyan) "$__fish_prompt_hostname" (set_color normal) "]"
end
