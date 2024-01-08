{ pkgs, ... }:
{
  boot.kernelPackages = pkgs.linuxPackages_latest;

  nixpkgs = {
    config.allowUnfree = true;
    config.allowUnfreePredicate = _: true;
    overlays = [
        (import (builtins.fetchTarball {
            url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
        }))
    ];
  };

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

    enableAllFirmware = true;
    enableRedistributableFirmware = true;

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
  };

  programs = {
    dconf.enable = true;

    hyprland = {
      enable = true;
      xwayland.enable = true;
    };

    zsh.enable = true;
  };

  environment = {
    # bootstrap
    systemPackages = with pkgs; [
      home-manager
      git
      gh
      greetd.tuigreet

      libsForQt5.qt5.qtwayland
      qt6.qtwayland

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

    variables.EDITOR = "nvim";
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
      dejavu_fonts
      hack-font
      font-awesome
      nerdfonts
      jetbrains-mono
    ];
  };

  services = {
    blueman.enable = true;

    dbus.enable = true;

    greetd = {
      enable = true;
      settings = {
        default_session.command = ''
          ${pkgs.greetd.tuigreet}/bin/tuigreet \
            --time \
            --asterisks \
            --user-menu \
            --cmd Hyprland
        '';
      };
    };

    udisks2.enable = true;

    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };

    printing.enable = true;
  };

  environment.etc."greetd/environments".text = '' hyprland '';

  systemd.services.greetd.serviceConfig = {
    Type = "idle";
    StandardInput = "tty";
    StandardOutput = "tty";
    StandardError = "journal";
    TTYReset = true;
    TTYVHangup = true;
    TTYVTDisallocate = true;
  };

  xdg = {
    autostart.enable = true;
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal
        xdg-desktop-portal-gtk
        xdg-desktop-portal-hyprland
      ];
    };
  };

  security = {
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
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
