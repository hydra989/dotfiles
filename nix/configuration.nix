{ config, lib, pkgs, ... }:
let
  home-manager = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/master.tar.gz";
  };
in
{
  imports = [
    (import "${home-manager}/nixos")
    ./hardware-configuration.nix
    ./active-machine.nix
    ./home.nix
            ];

  boot = {
    loader.efi.canTouchEfiVariables = true;
    loader.systemd-boot.enable = true;
    supportedFilesystems = [ "ntfs" ];
  };

  nix = {
    optimise.automatic = true;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  nixpkgs = {
    config = {
      allowUnfree = true;
      pulseaudio = true;
    };

    localSystem.system = "x86_64-linux";
  };

  networking = {
    useDHCP = false;
    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };
    wireless.iwd.enable = true;
  };

  sound.enable = true;

  hardware = {
    opengl.driSupport = true;
    opengl.driSupport32Bit = true;

    bluetooth.enable = true;

    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };

  # virt-manager
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

  # emacs
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];
  services.emacs.package = pkgs.emacsNativeComp;

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

  services = {
    xserver = {
      enable = true;
      layout = "us";

      displayManager = {
        lightdm = {
          enable = true;
          greeter.enable = true;
        };
      };

      windowManager.awesome.enable = true;
    };

    # flatpak.enable = true;
    blueman.enable = true;
    devmon.enable = true;
    picom = {
      enable = true;
      backend = "glx";
      vSync = true;
    };
    printing.enable = true;
  };

  users.users.hydra = {
    isNormalUser = true;
    home = "/home/hydra";
    extraGroups = [
      "wheel" "networkmanager" "video" "audio" "libvirtd"
    ];
    initialPassword = "rorschach";
    shell = pkgs.zsh;
  };

  security = {
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
  };

  # localization/defaults
  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  system.stateVersion = "22.05";
}
