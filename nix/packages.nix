{ config, lib, pkgs, ... }:
let
  masterTarball =
    fetchTarball
      https://github.com/NixOS/nixpkgs/archive/master.tar.gz;
in
{
  nixpkgs = {
    config = {
      allowUnfree = true;
      packageOverrides = pkgs: rec {
        deity = pkgs.callPackage ./packages/deity/default.nix {};

        masterNixpkgs = import masterTarball {
          config = config.nixpkgs.config;
        };
      };
      pulseaudio = true;
    };
    overlays = [
      # for emacs
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];
  };

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
    firefox xfce.thunar xfce.thunar-archive-plugin
    scrot feh pywal keepassxc zathura

    # media
    calibre mpv kodi torrential

    # dev tools
    git gh virt-manager docker nixpkgs-review

    # languages
    python3 pylint            # python
    gcc gdb bear clang-tools  # c/c++
    go gopls                  # go
    jdk11                     # java

    # tui
    tty-clock neofetch tor killall
    unzip lm_sensors wrap

    # games
    dwarf-fortress cataclysm-dda heroic minecraft
    lutris

    # custom packages
    deity

    # master branch
    masterNixpkgs.warpd masterNixpkgs.popcorntime
    masterNixpkgs.tidal-hifi

    # dependencies
    ghostscript
  ];

  virtualisation = {
    # docker
    docker = {
      enable = true;
      enableOnBoot = true;
    };
    # virt-manager
    libvirtd.enable = true;
  };

  # for virt-manager
  programs.dconf.enable = true;

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      dejavu_fonts
      hack-font
      terminus_font
      font-awesome
      nerdfonts
    ];
  };
}
