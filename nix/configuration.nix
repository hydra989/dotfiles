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
  nixpkgs.overlays = [ (import ./overlay/overlay.nix {}) ];

  networking = {
    useDHCP = false;
    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };
    wireless.iwd.enable = true;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  
  environment.systemPackages = with pkgs; [
    # dev tools
    emacs git gh vim
    # gui
    firefox kitty rofi calibre deluge vlc pywal
    # languages
    python3
    # games
    cataclysm-dda
    # tui
    tty-clock thefuck
  ];

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [ dejavu_fonts hack ];
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

    flatpak.enable = true;
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
