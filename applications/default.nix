{ pkgs, ... }: {
  imports = [
    ./alacritty
    ./firefox
    ./neovim
    ./virt-manager
    ./waybar
    ./zsh
  ];

  home.packages = with pkgs; [
    # === gui ===
    hyprpaper
    albert
    bitwarden
    zathura
    netflix
    spotify
    discord
    betterdiscordctl
    libreoffice
    networkmanagerapplet
    planify
    wlogout
    xfce.thunar
    xfce.thunar-archive-plugin

    # === tui ===
    neofetch
    unzip
    comma
    warpd
    fzf
    ripgrep

    # === media ===
    calibre
    mpv
    torrential
    popcorntime

    # === dev tools ===
    virt-manager
    git
    gh
    tree-sitter
    # kvm-osx ===
    libguestfs
    p7zip
    dmg2img
    # nix ===
    nil
    nixfmt
    nixpkgs-review
    nixpkgs-fmt
    # python
    python3
    python3Packages.pip
    python3Packages.python-lsp-server
    virtualenv
    pylint
    python3Packages.rope
    python3Packages.flake8
    # c/c++ ===
    gcc
    gdb
    clang-tools
    valgrind
    # go ===
    go
    gopls
    # java ===
    jdk11
    # latex ===
    texlive.combined.scheme-full
    auctex
    # js/typescript ===
    nodejs
    nodePackages_latest.typescript
    nodePackages_latest.typescript-language-server
    # lua ===
    lua-language-server

    # === games ===
    dwarf-fortress
    heroic
    wineWowPackages.stagingFull
    winetricks
    protontricks
    steam    
    crispy-doom
  ];
}
