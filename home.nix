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
        ".config/rofi/launchpad-theme.rasi".source = ./applications/rofi/launchpad-theme.rasi;
        ".xinitrc".source = ./.xinitrc;
    };

    sessionVariables = {
      CALIBRE_USE_DARK_PALETTE = "1"; # 1 = dark theme calibre
    };

    stateVersion = "23.11";
  };

  gtk = {
    enable = true;
    iconTheme = {
      name = "WhiteSur";
      package = pkgs.whitesur-icon-theme;
    };
    theme = {
      name = "WhiteSur";
      package = pkgs.whitesur-gtk-theme;
    };
  };

  xdg.configFile = {
    "nvim".source = ./.config/nvim;
  };
}
