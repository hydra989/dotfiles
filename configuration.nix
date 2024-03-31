{ pkgs, ... }:
{
  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking = {
    useDHCP = false;
    networkmanager = {
      enable = true;
    };
  };

  sound = {
    enable = true;
    mediaKeys = {
      enable = true;
      volumeStep = "5%";
    };
  };

  hardware = {
    bluetooth.enable = true;

    pulseaudio.enable = false;

    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
  };

  nix = {
    optimise.automatic = true;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings.trusted-users = [ "root" "hydra" ];
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  programs = {
    dconf.enable = true;
    zsh.enable = true;
  };

  environment = {
    # bootstrap
    systemPackages = with pkgs; [
      home-manager
      git
      gh
      # === custom packages ===
      # (pkgs.callPackage ./packages/deity.nix { })
    ];

    # as per zsh home-manager module
    pathsToLink = [ "/share/zsh" ];

    # settings for Proton-GE
    sessionVariables = {
      XDG_CACHE_HOME = "\${HOME}/.cache";
      XDG_CONFIG_HOME = "\${HOME}/.config";
      XDG_BIN_HOME = "\${HOME}/.local/bin";
      XDG_DATA_HOME = "\${HOME}/.local/share";
      # Steam needs this to find Proton-GE
      STEAM_EXTRA_COMPAT_TOOLS_PATHS =
        "\${HOME}/.steam/root/compatibilitytools.d";
      # note: this doesn't replace PATH, it just adds this to it
      PATH = [ "\${XDG_BIN_HOME}" ];
    };
  };

  virtualisation = {
    # virt-manager
    libvirtd.enable = true;
  };

  # for osx-kvm
  #boot.extraModprobeConfig = ''
  #  options kvm_intel nested=1
  #  options kvm_intel emulate_invalid_guest_state=0
  #  options kvm ignore_msrs=1
  #'';

  fonts = {
    fontDir.enable = true;
    packages = with pkgs; [
      hack-font
      font-awesome
      nerdfonts
      jetbrains-mono
    ];
  };

  services = {
    blueman.enable = true;

    dbus.enable = true;

    xserver = {
      enable = true;

      windowManager = {
        dwm = {
          enable = true;
          package = pkgs.dwm.override {
            conf = ./config.def.h;
          };
        };
      };
    };

    udisks2.enable = true;

    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };

    printing.enable = true;

    # autodiscovery for network printers
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
  };

  security = {
    sudo = {
      enable = true;
      wheelNeedsPassword = false;
    };
  };

  users.users.hydra = {
    isNormalUser = true;
    extraGroups =
      [ "wheel" "video" "audio" "libvirtd" "docker" ];
    initialPassword = "rorschach";
    shell = pkgs.zsh;
  };

  time.timeZone = "America/New_York";

  system.stateVersion = "23.11";
}
