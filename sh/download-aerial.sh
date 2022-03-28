#!/bin/bash

# download-aerial.sh
#
# download the wallpapers for aerial-sddm-theme.
# should be ran as superuser


mkdir videos
mkdir videos/day
mkdir videos/night

cat /usr/share/sddm/themes/aerial-sddm-theme/playlists/day.m3u | while read line
do
	wget $line videos/day/
done

cat /usr/share/sddm/themes/aerial-sddm-theme/playlists/night.m3u | while read line
do
	wget $line videos/night/
done
