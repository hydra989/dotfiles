{ config, pkgs, ... }:
{
  home-manager.users.hydra = {
    programs.home-manager.enable = true;
    home.stateVersion = "22.05";
    home.homeDirectory = "/home/hydra";

    # dotfiles
    home.file = {
      ".vimrc".source = ../.vimrc;
      ".zshrc".source = ../.zshrc;
      ".wallpaper".source = ../.wallpaper;
      ".emacs.d/init.el".source = ../.emacs.d/init.el;
      ".emacs.d/src".source = ../.emacs.d/src;
    };

    xdg.configFile = {
      "alacritty/alacritty.yml".source = ../.config/alacritty/alacritty.yml;
      "awesome/rc.lua".source = ../.config/awesome/rc.lua;
      "polybar/config.ini".source = ../.config/polybar/config.ini;
      "rofi/config.rasi".source = ../.config/rofi/config.rasi;
    };
  };
}
