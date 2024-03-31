{ pkgs, ... }:
{
  networking = {
    hostName = "songbird";
    interfaces.wlan0.useDHCP = true;
  };

  boot.initrd.kernelModules = [ "amdgpu" ];

  services = {
    # for display brightness keys
    illum.enable = true;

    # amdgpu
    xserver = {
      videoDrivers = [ "amdgpu" ];
      libinput.enable = true;
    };

    upower.enable = true;
  };

  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 20;
    };
  };

  hardware.opengl = {
    extraPackages = with pkgs; [
      rocm-opencl-icd
      rocm-opencl-runtime
      amdvlk
    ];
  };
}
