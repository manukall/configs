xrdb -load ~/.Xresources

(sleep 30 && dropboxd) &

urxvt &

DEFAULT_SESSION=awesome
case $1 in
  awesome) exec awesome ;;
  wmii) rvm ruby-1.9.2-p0@wmii exec wmii ;;
  xmonad) 
    xsetroot -cursor_name left_ptr;
    xmonad ;;
  *) exec $DEFAULT_SESSION ;;
esac
