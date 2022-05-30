{ config, pkgs, ... }:
{
  networking = {
    hostName = "canary";
  };

  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.enable = true;
}
