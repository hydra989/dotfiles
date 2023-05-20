{ pkgs, ... }: {
  imports = [ ./applications ];

  home = {
    username = "hydra";
    homeDirectory = "/home/hydra";

    file = {
      ".tmux.conf".source = ./.tmux.conf;
      ".tmux-powerlinerc".source = ./.tmux-powerlinerc;
      ".vimrc".source = ./.vimrc;
      ".wallpaper".source = ./.wallpaper;
      ".emacs.d/init.el".source = ./.emacs.d/init.el;
      ".emacs.d/config.org".source = ./.emacs.d/config.org;
    };

    sessionVariables = {
      CALIBRE_USE_DARK_PALETTE = "1"; # 1 = dark theme calibre
    };

    stateVersion = "22.11";
  };

  gtk = {
    enable = true;
    theme = {
      name = "Equilux";
      package = pkgs.equilux-theme;
    };
  };

  programs = {
    eww = {
      enable = true;
      configDir = ./.config/eww;
    };
    # redundant? also defined in configuration.nix
    zsh.enable = true;
  };
}
