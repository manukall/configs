xrdb -load ~/.Xresources

trayer --edge top --align right --SetDockType true --SetPartialStrut true \
--expand true --width 10 --transparent true --tint 0x191970 --height 12&

xsetroot -solid midnightblue
xsetroot -cursor_name left_ptr

dropboxd &

xmonad
