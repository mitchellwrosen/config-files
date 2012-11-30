#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Key repeat delay and rate
xset r rate 200 50

# Get them sexy fonts.
xset +fp /usr/share/fonts/local
xset fp rehash

# Get them sexy key mappings.
xmodmap ~/.Xmodmap

# Get them sexy key bindings.
xbindkeys

# Merge changes to .Xresources
xrdb -merge ~/.Xresources

exec awesome
