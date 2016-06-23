set -e

if [ $# -ne 2 ]; then
  echo "Usage: links.sh <home dir> <this dir>"
  exit 1
fi

HOMEDIR=$1
THISDIR=$2

function go() {
  TARGET=$1
  LINKNAME=$2

  read -p "Symlink $LINKNAME to $TARGET? (y/n) " yn
  case $yn in
    [Yy]* ) ln -s -f $TARGET $LINKNAME;;
    *     ) ;;
  esac
}

go $THISDIR/.dircolors $HOMEDIR/.dircolors
go $THISDIR/.gitconfig $HOMEDIR/.gitconfig
go $THISDIR/.xinitrc $HOMEDIR/.xinitrc
go $THISDIR/.xmonad $HOMEDIR/.xmonad
go $THISDIR/.Xmodmap $HOMEDIR/.Xmodmap
go $THISDIR/.Xresources $HOMEDIR/.Xresources
go $THISDIR/.zshrc $HOMEDIR/.zshrc
go $THISDIR/zsh/plugins/zsh-history-substring-search.zsh $HOMEDIR/.config/zsh/plugins/zsh-history-substring-search.zsh
go $THISDIR/nvim $HOMEDIR/.config/nvim
go $THISDIR/xorg.conf /etc/X11/xorg.conf
