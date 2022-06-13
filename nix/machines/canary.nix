{ config, pkgs, ... }:
{
  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;

  networking = {
    hostName = "canary";
    interfaces.wlan0.useDHCP = true;
    interfaces.enp42s0.useDHCP = true;
  };

  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.enable = true;

  environment.systemPackages = with pkgs; [
    unstable.lutris
  ];
}
