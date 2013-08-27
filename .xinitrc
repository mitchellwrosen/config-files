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

xset r rate 180 80               # Key repeat delay and rate
xset +fp /usr/share/fonts/local
xset fp rehash
#xset -b                          # No beep
xmodmap ~/.Xmodmap               # Key mappings
xbindkeys                        # Key bindings
xrdb -merge ~/.Xresources

#trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 3 --transparent true --alpha 10 --tint 0x000000 --heighttype pixel --height 10

exec xmonad
