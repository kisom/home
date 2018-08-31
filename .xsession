xscreensaver -no-splash &
if [ -f "$HOME/.fehbg" ]; then
	source "$HOME/.fehbg"
else
	xsetroot -solid "#000000"
fi
xmodmap $HOME/.xmodmaprc
exec ssh-agent /usr/local/bin/stumpwm
