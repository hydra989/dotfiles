#!/bin/sh

# (hopefully?) toggle waybar on/off.

if [ pgrep -x waybar >/dev/null ]
then
    # waybar is running, kill it
    pkill -SIGUSR1 '^waybar$'
else
    # waybar is not running, start it
    waybar &
fi
