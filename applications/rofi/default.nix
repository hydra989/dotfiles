{ config, ... }:
{
  programs.rofi = {
    enable = true;

    font = "Terminus 12";
    theme = "${config.xdg.cacheHome}/wal/colors-rofi-dark.rasi";
    extraConfig = {
      modi = "drun";
    };
  };
}
