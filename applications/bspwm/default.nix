{ config, ... }: {
  home.file."./.config/bspwm/bspwmrc" = {
    text = ''
      #!/bin/sh

      # hhydraa: credits to rxyhn, which is where I found this
      # rxyhn: Credits to 6gk/polka
      rule() { bspc rule -a "$@" & }
      config() { bspc config "$@" & }


      # =====================================================
      # config

      if [[ $(hostname) = "nightingale" ]]; then
         bspc monitor DP-4 -d 1 2 3 4 5 6
         bspc monitor HDMI-0 -d 7 8 9
         config -m DP-4 left_padding 69
         eww daemon && eww open bar &
      fi
      if [[ $(hostname) = "songbird" ]]; then
         bspc monitor eDP -d 1 2 3 4 5 6
         eww daemon && eww open laptopbar &
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


      # =====================================================
      # rules

      # floating
      rule Arandr           state=floating
      rule Bluetooth			  state=floating
      rule Thunar				    state=floating
      rule 'Tor Browser'	  state=floating

      # steam tag
      rule Steam   desktop=4  state=floating
      rule heroic  desktop=4  state=floating
      rule Lutris  desktop=4  state=floating
      # skyrim mod organizer
      rule steam_app_489830 desktop=4 state=floating

      # media tag
      rule calibre            desktop=5 state=floating
      rule vlc                desktop=5 state=floating
      rule Kodi               desktop=5 state=fullscreen
      rule mpv                desktop=5 state=floating
      rule Popcorn-Time       desktop=5 state=floating
      rule tidal-hifi         desktop=5 state=floating
      # TODO: torrential rule

      # socials tag
      # rule discord    desktop=6


      # =====================================================
      # autostart apps

      # clear cache
      rm ${config.xdg.configHome}/.cache/eww-calendar.lock

      wal -R &
    '';
    executable = true;
  };
}
