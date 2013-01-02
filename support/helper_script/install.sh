#!/bin/sh

if [ "$(whoami)" = "root" ]; then
	echo "Installing Nitrogen helper script."
	cp nitrogen.sh /usr/bin/
	chmod 755 /usr/bin/nitrogen.sh

	if [ -x /usr/bin/nitrogen ]; then
		echo "Warning: /usr/bin/nitrogen exists. Leaving intact."
	else
		ln -s /usr/bin/nitrogen.sh /usr/bin/nitrogen
	fi

	if [ -x /usr/bin/dev ]; then
		echo "Warning: /usr/bin/dev exist. Leaving intact."
	else
		ln -s /usr/bin/nitrogen.sh /usr/bin/dev
	fi
	echo "Script should be installed."
	echo "To invoke the script, you can use the 'nitrogen' or 'dev' commands"
	echo "from anywhere in a Nitrogen's installation."
else
	echo "Error: This must be run as root"
fi
