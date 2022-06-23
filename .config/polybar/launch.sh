#!/bin/sh

# taken from:
# github.com/MetaStag/awesome-dots

# kill running polybar instances
killall -q polybar
# wait until they're gone
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
# launch polybar
polybar -q topbar -c '~/.config/polybar/config.ini' &
