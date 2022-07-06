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
      epkgs.vterm epkgs.multi-vterm
    ]))
    songbird.picom
    songbird.polybar
    # songbird.rofi
    # songbird.vim
    songbird.wineWowPackages.staging
  ];

  environemt.sessionVariables = rec {
    EMACS_SERVER       = "n"; # use emacsclient/emacsserver?
    EMACS_EXWM         = "y"; # load exwm configuration?
    EMACS_TRANSPARENCY = "y"; # transparency on/off?
    EMACS_PYWAL        = "y"; # use theme-magic with pywal?
  };
}
