{ config, lib, pkgs, ... }:
{
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
      ohMyZsh = {
        enable = true;
        plugins = [ "git" ];
        theme = "ys";
      };
    };
  };

  environment.systemPackages = with pkgs; [
    # gui
    firefox alacritty calibre deluge vlc
    xfce.thunar maim feh pywal
    # dev tools
    git gh virt-manager
    # languages
    python3 pylint python-language-server
    gcc gdb bear clang-tools binutils
    # tui
    tty-clock thefuck neofetch tor
    # games
    dwarf-fortress cataclysm-dda
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
