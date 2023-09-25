{ pkgs, ... }: {
  home = {
    username = "hydra";
    homeDirectory = "/home/hydra";

    file = {
        ".config/hypr".source = ./.config/hypr;
        ".config/nvim".source = ./.config/neovim;
    };

    sessionVariables = {
        CALIBRE_USE_DARK_PALETTE = "1"; # 1 = dark theme calibre
        MOZ_ENABLE_WAYLAND = 1; # hint at firefox that we're on wayland
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
