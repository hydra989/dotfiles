{ config, pkgs, lib, ... }:
let
  canary = import <nixpkgs> {
    localSystem = {
      gcc.arch = "znver3";
      gcc.tune = "znver3";
      system = "x86_64-linux";
    };
  };
in
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

  services = {
    xserver.videoDrivers = [ "nvidia" ];

    emacs = {
      package = ((pkgs.emacsPackagesFor canary.emacsNativeComp).emacsWithPackages (epkgs: [ epkgs.vterm epkgs.multi-vterm ]));
      enable = true; # likely redundant
    };
  };

  hardware.opengl.enable = true;

  environment.systemPackages = [
    pkgs.lutris
    pkgs.alacritty # exwm on laptop, uses vterm in emacs

    # tunings
    ((pkgs.emacsPackagesFor canary.emacsNativeComp).emacsWithPackages (epkgs: [
      epkgs.vterm epkgs.multi-vterm
    ]))
    canary.picom
    canary.polybar
    canary.rofi
    canary.vim
    canary.wineWowPackages.staging
  ];
}
