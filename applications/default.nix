{ pkgs, lib, specialArgs, ... }: {
  imports = [
    ./nvim
    ./tmux
    ./zsh
  ] ++ lib.optionalAttrs specialArgs.isLinux [
    ./firefox
    ./foot
    ./virt-manager
    ./waybar
  ];

  nixpkgs = {
    config = {
        permittedInsecurePackages = [
            "electron-24.8.6"
        ];
        warpd = {
            withWayland = true;
            withX = false;
        };
    };
  };

  home.packages = with pkgs; [
    # === gui ===
    bitwarden
    zathura
    spotify
    discord
    betterdiscordctl
    libreoffice
    morgen
    pywal

    # === tui ===
    ripgrep
    eza
    highlight

    # === media ===
    mpv
    popcorntime

    # === dev tools ===
    git
    gh
    tree-sitter
    # nix ===
    nil
    nixfmt
    nixpkgs-review
    nixpkgs-fmt
    # python
    python3
    python3Packages.pip
    python3Packages.python-lsp-server
    pylint
    python3Packages.rope
    # c/c++ ===
    gcc
    clang-tools
    # go ===
    go
    gopls
    # java ===
    jdk11
    # latex ===
    texlive.combined.scheme-full
    auctex
    # lua ===
    lua-language-server
    # js ===
    nodejs
    nodePackages.npm
    biome

    # === games ===
    heroic
    wineWowPackages.stagingFull
    winetricks
    protontricks
    steam
  ] ++ lib.optionalAttrs specialArgs.isLinux [
    # hyprland things

    swaynotificationcenter
    hyprpaper
    swayimg
    networkmanagerapplet
    wlogout

    ulauncher
    xfce.thunar
    xfce.thunar-archive-plugin

    # linux specifics

    tofi
    netflix
    ranger
    neofetch
    warpd
    comma
    unzip
    virt-manager
    widevine-cdm
  ];
}
