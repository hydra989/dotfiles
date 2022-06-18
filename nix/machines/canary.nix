{ config, pkgs, lib, ... }:
{
  imports = [
    ./hardware-configuration-canary.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;

  nix.systemFeatures = [ "gccarch-znver3" ];

  nixpkgs.localSystem = lib.recursiveUpdate
    (lib.systems.elaborate {
      system = "x86_64-linux";
    }) {
      platform.gcc.arch = "znver3";
      platform.gcc.tune = "znver3";
    };

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
