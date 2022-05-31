{ config, pkgs, ... }:
{
  networking = {
    hostName = "canary";
    interfaces.wlan0.useDHCP = true;
  };

  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.enable = true;
}
