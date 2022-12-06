#
# home-manager configuration for hydra@songbird
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
      ".config/alacritty/alacritty.yml".source = ../../.config/alacritty/alacritty-little-font.yml;
    };

    sessionVariables = rec {
      EMACS_SERVER             = "n"; # use emacsclient/emacsserver?
      EMACS_TRANSPARENCY       = "y"; # transparency on/off?
      EMACS_PYWAL              = "y"; # use theme-magic with pywal?
      CALIBRE_USE_DARK_PALETTE = "1"; # 1 = dark theme calibre
    };

    stateVersion = "22.11";
  };

  xdg.configFile = {
    "awesome".source = ../../.config/awesome;
    "nixpkgs".source = ../../.config/nixpkgs;
    "polybar".source = ../../.config/polybar;
    "rofi".source = ../../.config/rofi;
  };
}
