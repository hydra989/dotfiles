{ pkgs, lib, specialArgs, ... }: {
  imports = [
    ./alacritty
    ./emacs
    ./nvim
    ./tmux
    ./zsh
  ] ++ lib.optionalAttrs specialArgs.isLinux [
    ./firefox
    ./qutebrowser
    ./picom
    ./rofi
    ./virt-manager
  ];

  nixpkgs = {
    config = {
      permittedInsecurePackages = [
        "electron-25.9.0"
      ];
      warpd = {
        withWayland = false;
        withX = true;
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
    pywal
    obsidian

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
    jdt-language-server
    # latex ===
    texlive.combined.scheme-full
    auctex
    # lua ===
    lua-language-server

    # === games ===
    heroic
    wineWowPackages.stagingFull
    winetricks
    protontricks
    steam
  ] ++ lib.optionalAttrs specialArgs.isLinux [
    scrot
    nitrogen
    netflix
    ranger
    neofetch
    warpd
    comma
    zip
    unzip
    virt-manager
    widevine-cdm
  ] ++ [
    # class-specifics
    bluej
    jetbrains.idea-community
  ];
}
