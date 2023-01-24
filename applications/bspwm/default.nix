{ config, inputs, pkgs, ... }: {
  home.file."./.config/bspwm/bspwmrc" = {
    text = ''
      #!/bin/sh

      if [[ $(hostname) = "nightingale" ]]; then
         bspc monitor DP-4 -d "0x1" "0x2" "0x3" "0x4" "steam" "media"
         bspc monitor HDMI-0 -d "0x5" "0x6" "0x7" "0x8"
      fi
      if [[ $(hostname) = "songbird" ]]; then
         bspc monitor eDP -d "0x1" "0x2" "0x3" "0x4" "steam" "media"
      fi

      bspc config border_width    2
      bspc config window_gap      5

      bspc config split_ratio								0.52
      bspc config borderless_monocle				true
      bspc config gapless_monocle						false
      bspc config remove_disabled_monitors 	true

      ## mouse settings
      bspc config focus_follows_pointer true
      bspc config pointer_modifier mod1
      bspc config pointer_action1 resize_side
      bspc config pointer_action1 resize_corner
      bspc config pointer_action3 move

      ## colors
      bspc config focused_border_color "#B3B3B3"
      bspc config normal_border_color  "#061115"

      ## rules

      # floating
      bspc rule -a Arandr state=floating
      bspc rule -a Bluetooth state=floating
      bspc rule -a Thunar state=floating
      bspc rule -a 'Tor Browser' state=floating

      # steam tag
      bspc rule -a Steam desktop="steam" state=floating
      bspc rule -a heroic desktop="steam" state=floating
      bspc rule -a Lutris desktop="steam" state=floating
      # skyrim mod organizer
      bspc rule -a steam_app_489830 desktop="steam" state=floating

      # media tag
      bspc rule -a calibre desktop="media" state=floating
      bspc rule -a vlc desktop="media" state=floating
      bspc rule -a Kodi desktop="media" state=fullscreen
      bspc rule -a mpv desktop="media" state=floating
      #bspc rule -a Popcorn-Time desktop="media" state=floating
      #bspc rule -a tidal-hifi desktop="media" state=floating
      # TODO: torrential rule

      #h=$(hostname) && sh ~/.screenlayout/$h.sh &
      sh ${config.xdg.configHome}/polybar/launch.sh &
    '';
    executable = true;
  };
}
