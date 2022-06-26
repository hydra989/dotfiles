{ config, lib, pkgs, ... }:
{
  nixpkgs = {
    config = {
      allowUnfree = true;
      pulseaudio = true;
    };
    overlays = [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];
  };

  # virt-manager
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

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
      syntaxHighlighting.enable = true;
      autosuggestions.enable = true;
      ohMyZsh = {
        enable = true;
        plugins = [ "git" ];
        theme = "ys";
      };
    };
  };

  environment.systemPackages = with pkgs; [
    # gui
    firefox calibre deluge vlc
    xfce.thunar maim feh pywal keepassxc

    # dev tools
    git gh virt-manager

    # languages
    python3 python3Packages.pip pylint
    gcc gdb bear clang-tools
    npm

    # tui
    tty-clock thefuck neofetch tor killall

    # games
    dwarf-fortress cataclysm-dda
  ];

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      dejavu_fonts hack-font terminus_font font-awesome nerdfonts
    ];
  };

  # enable for flatpak
  # xdg.portal = {
  #   enable = true;
  #   extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  # };
  # services.flatpak.enable = true;
}
