#
# home-manager configuration for hydra@songbird
#

{ inputs, lib, config, pkgs, ...}:
{
  programs.home-manager.enable = true;

  home = {
    username = "hydra";
    homeDirectory = "/home/hydra";

    file = {
      ".vimrc".source = ../../.vimrc;
      ".zshrc".source = ../../.zshrc;
      ".wallpaper".source = ../../.wallpaper;
      ".emacs.d/init.el".source = ../../.emacs.d/init.el;
      ".emacs.d/src".source = ../../.emacs.d/src;
    };

    sessionVariables = rec {
      EMACS_SERVER             = "n"; # use emacsclient/emacsserver?
      EMACS_EXWM               = "y"; # load exwm configuration?
      EMACS_TRANSPARENCY       = "y"; # transparency on/off?
      EMACS_PYWAL              = "y"; # use theme-magic with pywal?
      CALIBRE_USE_DARK_PALETTE = "1"; # 1 = dark theme calibre
    };

    stateVersion = "22.05"; 
  };

  xdg.configFile = {
    "alacritty".source = ../../.config/alacritty;
    "nixpkgs".source = ../../.config/nixpkgs;
    "polybar".source = ../../.config/polybar;
    "rofi".source = ../../.config/rofi;
  };
}
