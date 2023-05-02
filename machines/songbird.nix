{ pkgs, ... }:
{
  networking = {
    hostName = "songbird";
    interfaces.wlan0.useDHCP = true;
  };

  boot.initrd.kernelModules = [ "amdgpu" ];

  powerManagement.enable = true;

  environment.systemPackages = with pkgs; [
    powertop
  ];

  services = {
    # for display brightness keys
    illum.enable = true;

    # amdgpu
    xserver = {
      videoDrivers = [ "amdgpu" ];
      libinput.enable = true;

      # lxqt
      desktopManager.gnome.enable = true;
    };

    tlp = {
      enable = true;
      settings = {
        CPU_SCALING_GOVERNOR_ON_AC="performance";
        CPU_SCALING_GOVERNOR_ON_BAT="schedutil";
      };
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
