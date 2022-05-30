{ config, pkgs, ... }:
{
  networking = {
    hostName = "songbird";
  };

  boot.initrd.kernelModules = [ "amdgpu" ]
  services.xserver.videoDrivers = [ "amdgpu" ];
  hardware.opengl = {
    driSupport = true;
    # may be needed for steam?
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      rocm-opencl-icd
      rocm-opencl-runtime
      amdvlk
    ];
  };
}
