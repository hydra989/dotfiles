#!/bin/sh

# start the apps the needed started on login

hyprpaper &
waybar &
QT_QPA_PLATFORM=wayland albert &
nm-applet --indicator &
blueman-applet &
swaync &
