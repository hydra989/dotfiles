{ pkgs, ... }: {
  imports = [ ./applications ];

  home = {
    username = "hydra";
    homeDirectory = "/home/hydra";

    file = {
      # to ensure that directory doesn't become fully read-only
      ".config/nvim/init.lua".source = ./.config/neovim/init.lua;
      ".config/nvim/lua".source = ./.config/neovim/lua;

      ".tmux.conf".source = ./.tmux.conf;
      ".tmux-powerlinerc".source = ./.tmux-powerlinerc;
      ".wallpaper".source = ./.wallpaper;
      ".emacs.d/init.el".source = ./.emacs.d/init.el;
    };

    sessionVariables = {
      CALIBRE_USE_DARK_PALETTE = "1"; # 1 = dark theme calibre
    };

    stateVersion = "23.05";
  };

  xdg.configFile = {
    "i3".source = ./.config/i3;
  };

  gtk = {
    enable = true;
    theme = {
      name = "Equilux";
      package = pkgs.equilux-theme;
    };
  };

  programs = {
    # redundant? also defined in configuration.nix
    zsh.enable = true;
  };
}
