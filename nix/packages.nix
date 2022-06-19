{ config, lib, pkgs, ... }:
{
  nixpkgs = {
    config = {
      allowUnfree = true;
      pulseaudio = true;
    };
  };

  # virt-manager
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

  # emacs
  services.emacs.package = pkgs.emacsNativeComp;
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  programs = {
    # steam
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };
    # zsh
    zsh = {
      enable = true;
      ohMyZsh = {
        enable = true;
        plugins = [ "git" ];
        theme = "ys";
      };
    };
  };

  environment.systemPackages = with pkgs; [
    # gui
    firefox alacritty rofi calibre deluge vlc pywal
    polybar picom xfce.thunar maim feh alacritty
    # dev tools
    git gh vim emacsNativeComp virt-manager
    # languages
    python3 pylint python-language-server
    gcc gdb bear clang-tools
    # tui
    tty-clock thefuck neofetch tor
    # games
    dwarf-fortress cataclysm-dda wineWowPackages.staging
  ];

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      dejavu_fonts hack-font terminus_font font-awesome
    ];
  };

  # enable for flatpak
  # xdg.portal = {
  #   enable = true;
  #   extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  # };
  # services.flatpak.enable = true;
}
