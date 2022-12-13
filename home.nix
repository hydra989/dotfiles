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
      ".zshrc".source = ./.zshrc;
      ".wallpaper".source = ./.wallpaper;
      ".emacs.d/init.el".source = ./.emacs.d/init.el;
      ".emacs.d/src".source = ./.emacs.d/src;
    };

    sessionVariables = rec {
      CALIBRE_USE_DARK_PALETTE = "1"; # 1 = dark theme calibre
    };

    stateVersion = "22.11";
  };

  xdg.configFile = {
    "awesome".source = ./.config/awesome;
    "nixpkgs".source = ./.config/nixpkgs;
    "polybar".source = ./.config/polybar;
  };
}
