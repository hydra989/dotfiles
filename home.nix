{ inputs, lib, config, pkgs, pkgs_master, home-manager, ... }:
{
  programs.home-manager.enable = true;

  imports = [
    ./applications
  ];

  home = {
    username = "hydra";
    homeDirectory = "/home/hydra";

    file = {
      ".tmux.conf".source = ./.tmux.conf;
      ".tmux-powerlinerc".source = ./.tmux-powerlinerc;
      ".vimrc".source = ./.vimrc;
      ".xprofile".source = ./.xprofile;
      ".wallpaper".source = ./.wallpaper;
      ".emacs.d/init.el".source = ./.emacs.d/init.el;
      ".emacs.d/config.org".source = ./.emacs.d/config.org;
    };

    sessionVariables = rec {
      CALIBRE_USE_DARK_PALETTE = "1"; # 1 = dark theme calibre
    };

    stateVersion = "22.11";
  };

  xdg.configFile = {
    "polybar".source = ./.config/polybar;
  };
}
