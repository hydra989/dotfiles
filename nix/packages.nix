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
    # dev tools
    git gh vim virt-manager emacsNativeComp
    # gui
    firefox alacritty rofi calibre deluge vlc pywal picom polybar xfce.thunar
    # languages
    python3 pylint python-language-server
    gcc gdb bear clang-tools
    # games
    dwarf-fortress cataclysm-dda wineWowPackages.staging
    # tui
    tty-clock thefuck neofetch tor feh maim
  ];

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [ dejavu_fonts hack-font terminus_font siji ];
  };

  # enable for flatpak
  # xdg.portal = {
  #   enable = true;
  #   extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  # };
  # services.flatpak.enable = true;
}
