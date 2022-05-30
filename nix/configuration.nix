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
  
  networking.useDHCP = false;
  
  environment.systemPackages = with pkgs; [
    # dev tools
    emacs git gh vim

    # gui
    firefox kitty rofi calibre deluge vlc feh

    # languages
    python3

    # games
    cataclysm-dda
  ];

  services = {
    xserver = {
      enable = true;

      displayManager = {
        lightdm = {
          enable = true;
          greeter.enable = true;
        };
      };

      windowManager.dwm.enable = true;
    };

    flatpak.enable = true;
  };
  
  users.users.hydra = {
    isNormalUser = true;
    home = "/home/hydra";
    extraGroups = [ "wheel" "networkmanager" "video" "audio" ];
  };
  
  time.timeZone = "America/New_York";
  
  system.stateVersion = "21.11";
}
