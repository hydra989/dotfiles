{ config, pkgs, lib, ... }:
{
  imports = [
    ./hardware-configuration-canary.nix
  ];

  nix.systemFeatures = [ "gccarch-znver3" "big-parallel" ];
  # TODO: tuned kernel? currently throwing error with nvidia.
  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;

  networking = {
    hostName = "canary";
    interfaces.wlan0.useDHCP = true;
    interfaces.enp42s0.useDHCP = true;
  };

  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.enable = true;

  environment.systemPackages = with pkgs; [
    lutris
  ];
}
