{ pkgs, ... }: {
  imports = [ ./applications ];

  home = {
    username = "hydra";
    homeDirectory = "/home/hydra";

    file = {
      ".config/nvim/init.lua".source = ./.config/neovim/init.lua;
      ".config/nvim/lua".source = ./.config/neovim/lua;
      ".tmux.conf".source = ./.tmux.conf;
      ".tmux-powerlinerc".source = ./.tmux-powerlinerc;
      ".wallpaper".source = ./.wallpaper;
    };

    sessionVariables = {
      CALIBRE_USE_DARK_PALETTE = "1"; # 1 = dark theme calibre
    };

    stateVersion = "23.11";
  };

  gtk = {
    enable = true;
    iconTheme = {
        name = "Papirus";
        package = pkgs.papirus-icon-theme;
    };
    theme = {
      name = "Equilux";
      package = pkgs.equilux-theme;
    };
  };

  programs = {
    home-manager.enable = true;
    zsh.enable = true;
  };
}
