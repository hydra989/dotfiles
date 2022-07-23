{ config, pkgs, ... }:
let
  home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/release-22.05.tar.gz";
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
      "alacritty".source = ../.config/alacritty;
      "awesome".source = ../.config/awesome;
      "nixpkgs".source = ../.config/nixpkgs;
      "polybar".source = ../.config/polybar;
      "rofi".source = ../.config/rofi;
    };
  };
}
