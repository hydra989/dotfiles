{ pkgs, ... }:
{
    networking = {
        hostName = "songbird";
        interfaces.wlan0.useDHCP = true;
    };

    boot.initrd.kernelModules = [ "amdgpu" ];

    powerManagement = {
        enable = true;
        powertop.enable = true;
    };

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

    services.auto-cpufreq = {
        enable = true;
        settings = {
            battery = {
                governor = "powersave";
                turbo = "never";
            };
            charger = {
                governor = "performance";
                turbo = "auto";
            };
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
