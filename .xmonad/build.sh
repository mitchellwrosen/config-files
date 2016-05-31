#!/bin/bash

set -e

stack build
mkdir -p bin
stack --local-bin-path bin install xmobar --flag xmobar:with_alsa --flag xmobar:with_iwlib --flag xmobar:with_xft
