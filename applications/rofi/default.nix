{ config, inputs, ... }:
{
  programs.rofi = {
    enable = true;

    font = "terminus 12";
    theme = "${config.xdg.cacheHome}/wal/colors-rofi-light.rasi";
    extraConfig = {
      modi = "drun";
    };
  };
}
