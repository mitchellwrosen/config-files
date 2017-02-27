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

exec /home/mitchell/.xmonad/xmonad 1>/home/mitchell/.xmonad/xmonad-stdout 2>/home/mitchell/.xmonad/xmonad-stderr
