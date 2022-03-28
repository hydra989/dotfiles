#!/bin/bash

# upkeep.sh
#
# a small upkeep script for my void linux
# system. doesn't do anything too crazy.


# packages
echo "updating packages..."
xbps-install -Syu
echo "removing orphaned packages..."
xbps-remove -yo

# flatpaks
echo "checking for flatpak updates..."
flatpak update
if [[ $? -ne 0 ]]; then
	echo "unable to update flatpaks."
fi

# old kernels
echo "purging old kernels..."
vkpurge rm all

# done
echo "done!"
sleep 2
