#!/bin/bash

set -e

install_guix() {
	pushd /tmp
	wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh
	chmod +x guix-install.sh
	sudo ./guix-install.sh
	rm guix-install.sh
	popd
}

if [ -f "$(which guix)" ]; then
	echo "guix is already available; skipping install"
else
	echo "guix is not installed; I'll start by downloading and running guix-install.sh"
	read -p "proceed with installation? [y/n] " -n1 -s response
	echo ""
	if [ "${response}" != "y" ]; then
		exit 1
	fi

	install_guix
	guix pull
fi

if [ -z "$GUIX_PROFILE" ]; then
	GUIX_PROFILE="/home/briankubisiak/.config/guix/current"
	. "$GUIX_PROFILE/etc/profile"
fi

echo "guix is now installed and updated"
read -p "proceed with reconfiguring your home directory? [y/n] " -n1 -s response
echo ""
if [ "${response}" != "y" ]; then
	exit 2
fi

guix home reconfigure ./dotfiles.scm

# /etc/profile.d/guix.sh exports GUIX_LOCPATH=~/.guix-profile/lib/locale; this
# requires us to install locales here as well as through 'guix home'
guix install glibc-locales
