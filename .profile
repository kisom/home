# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export CVSROOT=anoncvs@anoncvs1.usa.openbsd.org:/cvs

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"

if [ "${SHELL##*/}" = "bash" ]
then
	[ -f ~/.fzf.bash ] && source ~/.fzf.bash
elif [ "${SHELL##*/}" = "zsh" ]
then
	[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
fi

# set up any additional scripts
PROFILES=$HOME/.profile.d
if [ -d $PROFILES ]
then
	for conf in $(ls $PROFILES/*.sh)
	do
		. $conf
	done
fi

ldrc () {
	source $PROFILES/${1}.sh
}

