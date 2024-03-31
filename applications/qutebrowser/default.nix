{ pkgs, ... }:
{
  programs.qutebrowser = {
    enable = true;
    extraConfig = ''
      import json
      import os

      c.scrolling.smooth = True
      c.colors.webpage.darkmode.enabled = True
    '';
  };
}
