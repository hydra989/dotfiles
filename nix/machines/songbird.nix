{ config, pkgs, ... }:
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
}
