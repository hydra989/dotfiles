{ config, pkgs, ... }:
{
  networking = {
    hostName = "songbird";
    # TODO: interfaces...useDHCP = true;
  };

  boot.initrd.kernelModules = [ "amdgpu" ];

  nixpkgs.localSystem = {
    gcc.arch = "znver1";
    gcc.tune = "znver1";
  };

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
}
