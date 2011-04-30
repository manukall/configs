xrdb -load ~/.Xresources

(sleep 30 && dropboxd) &

urxvt &

DEFAULT_SESSION=awesome
case $1 in
  awesome) exec awesome ;;
  wmii) rvm ruby-1.9.2-p0@wmii exec wmii ;;
  xmonad) 
    xsetroot -cursor_name left_ptr;
    trayer --edge top --align left --SetDockType true --SetPartialStrut true --expand true --width 10 --widthtype request --transparent true --tint 0x191970 --height 12 &
    volwheel &
    xmonad ;;
  *) exec $DEFAULT_SESSION ;;
esac
