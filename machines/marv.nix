{ config, pkgs, ... }:
{
    boot = {
        loader.grub = {
            enable = true;
            # for virt-manager vm install
            device = "/dev/vda";
            useOSProber = true;
        };
    };

    sound.enable = true;
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
        enable = true;
        alsa = {
            enable = true;
            support32Bit = true;
        };
        pulse.enable = true;
    };

    users.users.hayden = {
        isNormalUser = true;
        description = "hayden";
        extraGroups = [ "networkmanager" "wheel" ];
    };


    nixpkgs.config.allowUnfree = true;

    nix = {
        optimise.automatic = true;
        extraOptions = ''
            experimental-features = nix-command flakes
        '';
    };

    environment = {
        systemPackages = with pkgs; [
            git
            vim
            firefox
            calibre
        ];

        etc."nextcloud-admin-pass".text = "password123";
    };

    networking = {
        hostName = "marv";
        networkmanager.enable = true;
    };

    services = {
        # localhost port 80
        nextcloud = {
            enable = true;
            package = pkgs.nextcloud27;
            extraApps = with config.services.nextcloud.package.packages.apps; {
                inherit calendar tasks;
            };
            extraAppsEnable = true;
            hostName = "localhost";
            configureRedis = true;
            config.adminpassFile = "/etc/nextcloud-admin-pass";
        };

        # port 32400/web
        plex = {
            enable = true;
            openFirewall = true;
        };

        # port 8787
        readarr = {
            enable = true;
            openFirewall = true;
        };

        xserver = {
            enable = true;
            displayManager = {
                lightdm.enable = true;
                autoLogin = {
                    enable = true;
                    user = "hayden";
                };
            };
            desktopManager.xfce.enable = true;
            layout = "us";
            xkbVariant = "";
        };
    };

    security = {
        sudo = {
            enable = true;
            wheelNeedsPassword = true;
        };
    };

    time.timeZone = "America/New_York";

    system.stateVersion = "23.11";
}
