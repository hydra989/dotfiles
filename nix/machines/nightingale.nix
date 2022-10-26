{ config, lib, pkgs, ... }:
{
  virtualisation.docker.enableNvidia = true;

  networking = {
    hostName = "nightingale";
    interfaces.wlan0.useDHCP = true;
    interfaces.enp42s0.useDHCP = true;
  };

  services = {
    xserver = {
      videoDrivers = [ "nvidia" ];
      windowManager.awesome.enable = true;
    };

    syncthing = {
      enable = true;
      user = "hydra";
      dataDir = "/opt/syncthing";
      configDir = "/home/hydra/.config/syncthing";
    };
  };
}
