{ config, lib, pkgs, ... }:
{
  networking = {
    hostName = "songbird";
    interfaces.wlan0.useDHCP = true;
  };

  boot.initrd.kernelModules = [ "amdgpu" ];

  sound.mediaKeys = {
    enable = true;
    volumeStep = "5%";
  };

  services = {
    # for display brightness keys
    illum.enable = true;

    # amdgpu
    xserver = {
      videoDrivers = [ "amdgpu" ];
      libinput.enable = true;

      # lxqt
      desktopManager.lxqt.enable = true;

      # awesome
      windowManager.awesome.enable = true;
    };

    syncthing = {
      enable = true;
      user = "hydra";
      dataDir = "/home/hydra/syncthing";
      configDir = "/home/hydra/.config/syncthing";
    };

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
