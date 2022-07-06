{ config, pkgs, ... }:
let
  home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/master.tar.gz";
in
{
  imports = [
    (import "${home-manager}/nixos")
  ];

  users.users.hydra = {
    isNormalUser = true;
    extraGroups = [
      "wheel" "networkmanager" "video" "audio" "libvirtd" "docker"
    ];
    initialPassword = "rorschach";
    shell = pkgs.zsh;
  };

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
      # alacritty
      "alacritty/alacritty.yml".source = ../.config/alacritty/alacritty.yml;

      # awesomewm
      "awesome/rc.lua".source = ../.config/awesome/rc.lua;

      # nix
      "nixpkgs/config.nix".source = ../.config/nixpkgs/config.nix;

      # polybar
      "polybar/config.ini".source = ../.config/polybar/config.ini;
      "polybar/launch.sh".source = ../.config/polybar/launch.sh;

      # rofi
      "rofi/config.rasi".source = ../.config/rofi/config.rasi;
      "rofi/rofi-network-manager.rasi".source = ../.config/rofi/rofi-network-manager.rasi;
      "rofi/rofi-network-manager.conf".source = ../.config/rofi/rofi-network-manager.conf;
    };
  };
}
