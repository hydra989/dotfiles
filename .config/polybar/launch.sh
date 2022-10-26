#!/bin/sh

# taken from:
# github.com/MetaStag/awesome-dots

h=$(hostname)
# kill running polybar instances
killall -q polybar
# wait until they're gone
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
# launch polybar
polybar -q topbar -c '~/.config/polybar/$h.config.ini'
