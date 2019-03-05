xscreensaver -no-splash &
if [ -f "$HOME/.fehbg" ]; then
	source "$HOME/.fehbg"
else
	xsetroot -solid "#000000"
fi
xmodmap $HOME/.xmodmaprc

if [ -x /usr/local/bin/stumpwm ]
then
	exec ssh-agent /usr/local/bin/stumpwm
else
	exec ssh-agent /usr/bin/i3
fi
