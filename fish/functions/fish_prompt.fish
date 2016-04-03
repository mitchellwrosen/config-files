function fish_prompt --description 'Write out the prompt'
  set -l normal (set_color normal)

  set -l mode_color (
    switch $fish_bind_mode
      case default
        set_color --bold red
      case insert
        set_color --bold green
      case visual
        set_color --bold magenta
    end
  )

  echo -n -s $mode_color (prompt_pwd) $normal (__fish_git_prompt) $normal ' '
end
