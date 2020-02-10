export LC_CTYPE="en_US.UTF-8" 
if [ -x /usr/local/bin/dbus-launch -a -z "${DBUS_SESSION_BUS_ADDRESS}" ]; then
        eval `dbus-launch --sh-syntax --exit-with-x11`
fi

if [ -f "$HOME/.fehbg" ]; then
	. "$HOME/.fehbg"
else
	xsetroot -solid "#000000"
fi

xidle -sw -program /usr/local/bin/i3lock -timeout 600 &

# disable system beep
xset b off

# xmodmap $HOME/.xmodmaprc
setxkbmap -option compose:ralt -option ctrl:swapcaps 

# if you have a ThinkPad, the following lines enable TrackPoint
# scrolling by dragging with the middle button.
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation" 1 
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Button" 2 
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Axes" 6 7 4 5 

keychain -q &

if [ -x /usr/local/bin/stumpwm ]
then
	exec /usr/local/bin/stumpwm
else
	exec /usr/local/bin/i3
fi
