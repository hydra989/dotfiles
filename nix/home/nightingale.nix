#
# home-manager configuration for hydra@nightingale
#

{ inputs, lib, config, pkgs, pkgs_master, home-manager, ...}:
{
  programs.home-manager.enable = true; 

  home = {
    username = "hydra";
    homeDirectory = "/home/hydra";

    file = {
      ".tmux.conf".source = ../../.tmux.conf;
      ".tmux-powerlinerc".source = ../../.tmux-powerlinerc;
      ".vimrc".source = ../../.vimrc;
      ".zshrc".source = ../../.zshrc;
      ".wallpaper".source = ../../.wallpaper;
      ".emacs.d/init.el".source = ../../.emacs.d/init.el;
      ".emacs.d/src".source = ../../.emacs.d/src;
    };

    sessionVariables = rec {
      EMACS_SERVER             = "y"; # use emacsclient/emacsserver?
      EMACS_EXWM               = "n"; # load exwm configuration?
      EMACS_TRANSPARENCY       = "y"; # transparency on/off?
      EMACS_PYWAL              = "y"; # use theme-magic with pywal?
      CALIBRE_USE_DARK_PALETTE = "1"; # 1 = dark theme calibre
    };

    stateVersion = "22.05";
  };

  
  xdg.configFile = {
    "alacritty".source = ../../.config/alacritty;
    "awesome".source = ../../.config/awesome;
    "nixpkgs".source = ../../.config/nixpkgs;
    "polybar".source = ../../.config/polybar;
    "rofi".source = ../../.config/rofi;
  };
}
