{ config, pkgs, ... }:

let
  songbird = import <nixpkgs> {
    localSystem = {
      gcc.arch = "znver1";
      gcc.tune = "znver1";
      system = "x86_64-linux";
    };
  };
  home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/release-22.05.tar.gz";
in
{
  imports = [
    ../hardware/hardware-configuration-songbird.nix
    ../configuration.nix
    (import "${home-manager}/nixos")
  ];

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

      # gnome
      desktopManager.gnome.enable = true;
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

  environment = {
    systemPackages = with pkgs; [
      # tuning takes time
      #songbird.picom
      #songbird.polybar
      #songbird.vim
      #songbird.wineWowPackages.staging

      picom polybar vim wineWowPackages.staging rofi feh
    ];
    # gnome, but skip some packages
    gnome.excludePackages = (with pkgs.gnome; [
      gedit
      epiphany
      geary
      evince
      totem
      tali
      iagno
      hitori
      atomix
    ]);
  };

  home-manager.users.hydra = {
    programs.home-manager.enable = true;
    home.stateVersion = "22.05";
    home.homeDirectory = "/home/hydra";

    # dotfiles
    home.file = {
      ".vimrc".source = ../../.vimrc;
      ".zshrc".source = ../../.zshrc;
      ".wallpaper".source = ../../.wallpaper;
      ".emacs.d/init.el".source = ../../.emacs.d/init.el;
      ".emacs.d/src".source = ../../.emacs.d/src;

      # polybar's configuration changes based on machine
      ".config/polybar/launch.sh".source = ../../.config/polybar/launch.sh;
      ".config/polybar/config.ini".source = ../../.config/polybar/songbird.config.ini;
    };
    xdg.configFile = {
      "alacritty".source = ../../.config/alacritty;
      "nixpkgs".source = ../../.config/nixpkgs;
      "rofi".source = ../../.config/rofi;
    };
  };

  environment.sessionVariables = rec {
    EMACS_SERVER             = "n";   # use emacsclient/emacsserver?
    EMACS_EXWM               = "y";   # load exwm configuration?
    EMACS_TRANSPARENCY       = "y";   # transparency on/off?
    EMACS_PYWAL              = "y";   # use theme-magic with pywal?
    CALIBRE_USE_DARK_PALETTE = "1";   # 1 = dark theme calibre
    EDITOR                   = "vim";
  };
}
