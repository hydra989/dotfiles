{ config, ... }: {
  home.file."./.config/bspwm/bspwmrc" = {
    text = ''
      #!/bin/sh

      # hhydraa: credits to rxyhn, which is where I found this
      # Credits to 6gk/polka
      rule() { bspc rule -a "$@" & }
      config() { bspc config "$@" & }

      if [[ $(hostname) = "nightingale" ]]; then
         bspc monitor DP-4 -d 1 2 3 4 5 6
         bspc monitor HDMI-0 -d 7 8 9
      fi
      if [[ $(hostname) = "songbird" ]]; then
         bspc monitor eDP -d 1 2 3 4 5 6
      fi

      config border_width    2
      config window_gap      5

      config split_ratio 0.52
      config borderless_monocle true
      config gapless_monocle false
      config remove_disabled_monitors true
      config remove_unplugged_monitors true

      ## mouse settings
      config focus_follows_pointer true
      config pointer_modifier mod1
      config pointer_action1 resize_side
      config pointer_action1 resize_corner
      config pointer_action3 move

      ## colors
      config focused_border_color "#B3B3B3"
      config normal_border_color  "#061115"

      ## rules

      # floating
      rule -a Arandr				 state=floating
      rule -a Bluetooth			 state=floating
      rule -a Thunar				 state=floating
      rule -a 'Tor Browser'	 state=floating

      # steam tag
      rule -a Steam	desktop=4 state=floating
      rule -a heroic desktop=4 state=floating
      rule -a Lutris desktop=4 state=floating
      # skyrim mod organizer
      rule -a steam_app_489830 desktop=4 state=floating

      # media tag
      rule -a calibre	desktop=5 state=floating
      rule -a vlc desktop=5 state=floating
      rule -a Kodi desktop=5 state=fullscreen
      rule -a mpv desktop=5 state=floating
      #rule -a Popcorn-Time desktop=5 state=floating
      #rule -a tidal-hifi desktop=5 state=floating
      # TODO: torrential rule

      # =====================================================
      # autostart apps

      wal -R &
      eww -c ${config.xdg.configHome}/eww --restart open bar &
    '';
    executable = true;
  };
}
