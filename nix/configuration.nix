{ config, lib, pkgs, ... }: 
{
  imports = [
    ./hardware-configuration.nix
    ./active-machine.nix
            ];

  boot = {
    loader.efi.canTouchEfiVariables = true;
    loader.systemd-boot.enable = true;
  };

  nixpkgs.config.allowUnfree = true;
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
    emacs git gh vim
    # gui
    firefox kitty rofi calibre deluge vlc pywal picom steam
    # languages
    python3
    # games
    cataclysm-dda
    # tui
    tty-clock thefuck

    (dwm.overrideAttrs (oldAttrs: rec {
      src = fetchFromGitHub {
        owner = "hydra989";
        repo = "dwm-6.2-fork";
        rev = "25a1954a81c8c389591270ef0c7bd535663bca8d";
        sha256 = "sha256-36yYV7bDlb5cISlK4Ak6csuKint5ZVPQogCysHNcAhs=";
      };
    }))
  ];

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [ dejavu_fonts hack-font ];
  };

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

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

      windowManager.dwm.enable = true;
    };

    emacs.enable = true;
    flatpak.enable = true;
    picom.enable = true;
    printing.enable = true;
  };

  users.users.hydra = {
    isNormalUser = true;
    home = "/home/hydra";
    extraGroups = [ "wheel" "networkmanager" "video" "audio" ];
    initialPassword = "rorschach";
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = true;
  };
  
  time.timeZone = "America/New_York";
  
  system.stateVersion = "22.05";
}
