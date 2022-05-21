#!/bin/sh

# proxmox.sh
# do some setup-type things on a fresh proxmox install


# download dark theme
wget https://raw.githubusercontent.com/Weilbyte/PVEDiscordDark/master/PVEDiscordDark.py
python3 PVEDiscordDark.py


# update
apt update && apt dist-upgrade


# remind
echo "don't forget to setup the community subscription repository"
echo "and remove the web gui soft paywall!"
echo "happy hacking :)"
