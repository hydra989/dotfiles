{ config, pkgs, lib, ... }:
let
  canary = import <nixpkgs> {
    localSystem = {
      gcc.arch = "znver3";
      gcc.tune = "znver3";
      system = "x86_64-linux";
    };
  };
  home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/release-22.05.tar.gz";
in
{
  imports = [
    ./hardware-configuration-canary.nix
    ../configuration.nix
    (import "${home-manager}/nixos")
  ];

  nix.systemFeatures = [ "gccarch-znver3" "big-parallel" ];

  networking = {
    hostName = "canary";
    interfaces.wlan0.useDHCP = true;
    interfaces.enp42s0.useDHCP = true;
  };

  services = {
    xserver = {
      videoDrivers = [ "nvidia" ];
      windowManager.awesome.enable = true;
    };

    emacs = {
      package = ((pkgs.emacsPackagesFor canary.emacsNativeComp).emacsWithPackages (epkgs: [
        epkgs.use-package
        epkgs.vterm epkgs.multi-vterm
      ]));
      enable = true; # likely redundant
    };

    syncthing = {
      enable = true;
      user = "hydra";
      dataDir = "/opt/syncthing";
      configDir = "/home/hydra/.config/syncthing";
    };
  };

  hardware.opengl.enable = true;

  environment.systemPackages = [
    pkgs.alacritty    # exwm on laptop uses vterm within emacs
    pkgs.cmatrix      # vterm doesn't take kindly to cmatrix

    # tunings
    ((pkgs.emacsPackagesFor canary.emacsNativeComp).emacsWithPackages (epkgs: [
      epkgs.vterm epkgs.multi-vterm epkgs.use-package
    ]))
    canary.picom
    canary.polybar
    canary.rofi
    canary.vim
    canary.wineWowPackages.staging
  ];

  virtualisation.docker.enableNvidia = true;

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
      ".config/polybar/config.ini".source = ../../.config/polybar/canary.config.ini;
    };
    xdg.configFile = {
      "alacritty".source = ../../.config/alacritty;
      "awesome".source = ../../.config/awesome;
      "nixpkgs".source = ../../.config/nixpkgs;
      "rofi".source = ../../.config/rofi;
    };
  };

  environment.sessionVariables = rec {
    EMACS_SERVER             = "y"; # use emacsclient/emacsserver?
    EMACS_EXWM               = "n"; # load exwm configuration?
    EMACS_TRANSPARENCY       = "y"; # transparency on/off?
    EMACS_PYWAL              = "y"; # use theme-magic with pywal?
    CALIBRE_USE_DARK_PALETTE = "1";
    VISUAL                   = "emacsclient -c";
  };
}
