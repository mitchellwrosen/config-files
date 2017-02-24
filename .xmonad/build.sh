#!/bin/bash

set -e

stack build X11
stack build X11-xft
stack build xmonad
stack build xmonad-contrib
stack build alsa-core
stack build alsa-mixer

stack ghc -- xmonad.hs -Wall -O2

stack \
  --local-bin-path . \
  install xmobar \
  --flag xmobar:with_alsa \
  --flag xmobar:with_iwlib \
  --flag xmobar:with_xft
