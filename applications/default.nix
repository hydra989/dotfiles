{ config, lib, home-manager, ... }:
{
  imports = [
    ./alacritty
    ./bspwm
    ./rofi
    ./sxhkd
    ./zsh
  ];
}
