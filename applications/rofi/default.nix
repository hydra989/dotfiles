{ config, ... }:
{
  programs.rofi = {
    enable = true;

    font = "Terminus 12";
    theme = "${config.xdg.cacheHome}/wal/colors-rofi-light.rasi";
    extraConfig = {
      modi = "drun";
    };
  };
}
