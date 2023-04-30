{ config, lib, home-manager, ... }:
{
  imports = [
    ./alacritty
    ./bspwm
		./picom
    ./rofi
    ./sxhkd
    ./zsh
  ];
}
