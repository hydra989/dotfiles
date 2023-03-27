#!/bin/sh

# kill running polybar instances
killall -q polybar

# wait until they're gone
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# launch polybar
# stolen/modified from https://github.com/polybar/polybar/issues/763#issuecomment-331604987
for m in $(polybar --list-monitors | cut -d":" -f1); do
	MONITOR=$m polybar --reload leftbar_$(hostname) & \
		polybar --reload rightbar_$(hostname) &
	# TODO: condense into one line
done

if [ "$HOSTNAME" = "nightingale" ]; then
	polybar --reload leftbar_nightingale2 &
fi
