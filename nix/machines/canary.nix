{ config, pkgs, ... }:
{
  networking = {
    hostName = "canary";
    interfaces.wlo0.useDHCP = true;
    interfaces.enp42s0.useDHCP = true;
  };

  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.enable = true;
}
