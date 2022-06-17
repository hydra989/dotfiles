{ config, pkgs, ... }:
{
  home-manager.users.hydra = {
    home.file = {
      ".vimrc".source = ./s/dotfiles/.vimrc;
      ".zshrc".source = ./s/dotfiles/.zshrc;
      ".wallpaper".source = ./s/dotfiles/.wallpaper;
      ".emacs.d/init.el".source = ./s/dotfiles/.emacs.d/init.el;
      ".emacs.d/src".source = ./s/dotfiles/.emacs.d/src;
    };

    xdg.configFile = {
      "alacritty/alacritty.yml".source = ./s/dotfiles/.config/alacritty/alacritty.yml;
      "awesome/rc.lua".source = ./s/dotfiles/.config/awesome/rc.lua;
      "polybar/config.ini".source = ./s/dotfiles/.config/polybar/config.ini;
      "rofi/config.rasi".source = ./s/dotfiles/.config/rofi/config.rasi;
    };
  };
}
