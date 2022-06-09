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

  nix.optimise.automatic = true;

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };
  # nixpkgs.overlays = [ (import ./overlay/overlay.nix {}) ];

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

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  
  environment.systemPackages = with pkgs; [
    # dev tools
    unstable.emacs git gh vim
    # gui
    firefox kitty rofi calibre deluge vlc pywal picom
    # languages
    python3 pylint python-language-server
    gcc clang-tools # clangtools for clangd
    # games
    dwarf-fortress cataclysm-dda unstable.steam wineWowPackages.staging
    # tui
    tty-clock thefuck neofetch tor feh
  ];

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [ dejavu_fonts hack-font terminus_font ];
  };

  # enable for flatpak
  # xdg.portal = {
  #  enable = true;
  #  extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
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
    extraGroups = [ "wheel" "networkmanager" "video" "audio" ];
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
