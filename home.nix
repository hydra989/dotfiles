{ pkgs, ... }:
{
  nixpkgs.config.allowUnfree = true;

  programs = {
    home-manager.enable = true;
  };

  home = {
    username = "hydra";
    homeDirectory = "/home/hydra";

    file = {
        ".emacs.d/init.el".source = ./.emacs.d/init.el;
        ".emacs.d/themes".source = ./.emacs.d/themes;
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

  xdg.configFile = {
    "hypr".source = ./.config/hypr;
    "nvim".source = ./.config/nvim;
  };
}
