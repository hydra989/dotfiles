{ config, pkgs, ... }:
let
  songbird = import <nixpkgs> {
    localSystem = {
      gcc.arch = "znver1";
      gcc.tune = "znver1";
      system = "x86_64-linux";
    };
  };
in
{
  networking = {
    hostName = "songbird";
    # TODO: interfaces...useDHCP = true;
  };

  boot.initrd.kernelModules = [ "amdgpu" ];

  nix.systemFeatures = [ "gccarch-znver1" "big-parallel" ];

  services = {
    # amdgpu
    xserver = {
      videoDrivers = [ "amdgpy" ];
      libinput.enable = true;
    };

    syncthing = {
      enable = true;
      user = "hydra";
      dataDir = "/home/hydra/syncthing";
      configDir = "/home/hydra/.config/syncthing";
    };

    tlp.enable = true;
    upower.enable = true;
  };

  hardware.opengl = {
    extraPackages = with pkgs; [
      rocm-opencl-icd
      rocm-opencl-runtime
      amdvlk
    ];
  };

  environment.systemPackages = with pkgs; [
    ((pkgs.emacsPackagesFor songbird.emacsNativeComp).emacsWithPackages (epkgs: [
      epkgs.use-package
      epkgs.vterm epkgs.multi-vterm
    ]))
    songbird.picom
    songbird.polybar
    # songbird.rofi
    songbird.vim
    songbird.wineWowPackages.staging
  ];

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

  environment.sessionVariables = rec {
    EMACS_SERVER             = "y"; # use emacsclient/emacsserver?
    EMACS_EXWM               = "n"; # load exwm configuration?
    EMACS_TRANSPARENCY       = "y"; # transparency on/off?
    EMACS_PYWAL              = "y"; # use theme-magic with pywal?
    CALIBRE_USE_DARK_PALETTE = "1";
    EDITOR                   = "vim";
  };
}
