{ config, pkgs, ... }:
let
  songbird = import <nixpkgs> {
    localSystem = {
      gcc.arch = "znver1";
      gcc.tune = "znver1";
      system = "x86_64-linux";
    };
  };
in
{
  networking = {
    hostName = "songbird";
    # TODO: interfaces...useDHCP = true;
  };

  boot.initrd.kernelModules = [ "amdgpu" ];

  nix.systemFeatures = [ "gccarch-znver1" "big-parallel" ];

  services = {
    # amdgpu
    xserver = {
      videoDrivers = [ "amdgpy" ];
      libinput.enable = true;
    };

    tlp.enable = true;
    upower.enable = true;
  };

  hardware.opengl = {
    extraPackages = with pkgs; [
      rocm-opencl-icd
      rocm-opencl-runtime
      amdvlk
    ];
  };

  environment.systemPackages = with pkgs; [
    songbird.emacsNativeComp
    songbird.picom
    songbird.polybar
    # songbird.rofi
    # songbird.vim
    songbird.wineWowPackages.staging
  ];
}
