{ config, lib, pkgs, ... }: 
let
  unstableTarball =
    fetchTarball
      https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz;
in
{
  imports = [
    ./hardware-configuration.nix
    ./active-machine.nix
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
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };

  # emacs
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];
  services.emacs.package = pkgs.emacsNativeComp;

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

    pulseaudio.enable = true;
  };

  # for virt-manager
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  environment.systemPackages = with pkgs; [
    # dev tools
    git gh vim virt-manager
    # gui
    firefox kitty rofi calibre deluge vlc pywal picom
    # languages
    python3 pylint python-language-server
    gcc gdb clang-tools # clangtools for clangd
    # games
    dwarf-fortress cataclysm-dda unstable.steam wineWowPackages.staging
    # tui
    tty-clock thefuck neofetch tor feh maim
  ];

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [ dejavu_fonts hack-font terminus_font ];
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
  };

  security= {
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
  };

  time.timeZone = "America/New_York";

  system.stateVersion = "22.05";
}
