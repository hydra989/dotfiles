#!/bin/sh

# start the apps the needed started on login

hyprpaper &
waybar &
QT_QPA_PLATFORM=xcb albert &
nm-applet --indicator &
blueman-applet &
