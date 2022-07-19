{ config, pkgs, lib, ... }:
let
  canary = import <nixpkgs> {
    localSystem = {
      gcc.arch = "znver3";
      gcc.tune = "znver3";
      system = "x86_64-linux";
    };
  };
in
{
  imports = [
    ./hardware-configuration-canary.nix
  ];

  nix.systemFeatures = [ "gccarch-znver3" "big-parallel" ];

  networking = {
    hostName = "canary";
    interfaces.wlan0.useDHCP = true;
    interfaces.enp42s0.useDHCP = true;
  };

  services = {
    xserver.videoDrivers = [ "nvidia" ];

    emacs = {
      package = ((pkgs.emacsPackagesFor canary.emacsNativeComp).emacsWithPackages (epkgs: [
        epkgs.vterm epkgs.multi-vterm epkgs.use-package
      ]));
      enable = true; # likely redundant
    };

    syncthing = {
      enable = true;
      user = "hydra";
      dataDir = "/opt/syncthing";
      configDir = "/home/hydra/.config/syncthing";
    };

    deluge = {
      enable = true;
      openFirewall = true;
      web = {
        enable = true;
        openFirewall = true;
      };
    };
    #bazarr = {
      #enable = true;
      #openFirewall = true;
    #};
    #radarr = {
      #enable = true;
      #openFirewall = true;
    #};
    #sonarr = {
      #enable = true;
      #openFirewall = true;
    #};
  };

  hardware.opengl.enable = true;

  environment.systemPackages = [
    #pkgs.bazarr
    #pkgs.radarr
    #pkgs.sonarr
    pkgs.lutris
    pkgs.alacritty    # exwm on laptop uses vterm within emacs

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

  environment.sessionVariables = rec {
    EMACS_SERVER             = "y"; # use emacsclient/emacsserver?
    EMACS_EXWM               = "n"; # load exwm configuration?
    EMACS_TRANSPARENCY       = "y"; # transparency on/off?
    EMACS_PYWAL              = "y"; # use theme-magic with pywal?
    CALIBRE_USE_DARK_PALETTE = "1";
    VISUAL                   = "emacsclient -c";
  };
}
