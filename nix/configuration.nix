{ config, lib, pkgs, ... }:
{
  imports = [
    ./packages.nix
    ./machines/canary.nix
  ];

  boot = {
    cleanTmpDir = true;
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

  services = {
    blueman.enable = true;

    devmon.enable = true;

    picom = {
      enable = true;
      backend = "glx";
      vSync = true;
    };

    printing.enable = true;

    xserver = {
      enable = true;
      layout = "us";
      displayManager.lightdm.enable = true;
      windowManager.awesome.enable = true;
    };
  };

  security = {
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
  };

  users.users.hydra = {
    isNormalUser = true;
    extraGroups = [
      "wheel" "networkmanager" "video" "audio" "libvirtd" "docker"
    ];
    initialPassword = "rorschach";
    shell = pkgs.zsh;
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
