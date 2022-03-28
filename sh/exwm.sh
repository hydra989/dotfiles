# set default display
# export DISPLAY=:0.0

# picom
picom -b

# default wallpaper
feh --bg-scale ~/Git/dotfiles/wallpapers/nerv_evangelion.jpg

# emacs runs exwm if $EXWM is set to "true"
export EXWM="true"

# run emacs
/bin/emacs --fullscreen --no-site-file
