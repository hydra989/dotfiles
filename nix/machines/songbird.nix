{ config, lib, pkgs, ... }:
{
  networking = {
    hostName = "songbird";
    interfaces.wlan0.useDHCP = true;
  };

  boot.initrd.kernelModules = [ "amdgpu" ];

  nix.systemFeatures = [ "gccarch-znver1" "big-parallel" ];

  services = {
    # for display brightness keys
    illum.enable = true;

    # amdgpu
    xserver = {
      videoDrivers = [ "amdgpy" ];
      libinput.enable = true;

      # exwm
      windowManager.exwm = {
        enable = true;
        enableDefaultConfig = false;
        loadScript = ''
                   (require 'exwm)
                   (exwm-enable)
        '';
        extraPackages = epkgs: [
                 epkgs.use-package
                 epkgs.vterm epkgs.multi-vterm
                 epkgs.exwm
        ];
      };

      # lxqt
      lxqt.enable = true;

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
