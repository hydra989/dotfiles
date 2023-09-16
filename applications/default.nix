{ pkgs, ... }: {
  imports = [
    ./alacritty
    ./neovim
    ./rofi
    ./zsh
  ];

  home.packages = with pkgs; [
    # === gui ===
    firefox
    scrot
    keepassxc # for old passwords, i've since
    bitwarden # migrated to this
    zathura
    pywal
    tutanota-desktop
    netflix
    spotify
    discord
    betterdiscordctl

    # === tui ===
    tty-clock
    neofetch
    tor
    killall
    unzip
    lm_sensors
    tmux
    cmatrix
    comma
    warpd
    cbonsai
    fzf
    calcure
    neomutt

    # === media ===
    calibre
    mpv
    kodi
    torrential
    popcorntime

    # === dev tools ===
    git
    gh
    virt-manager
    docker
    nixpkgs-review
    tree-sitter
    # kvm-osx ===
    libguestfs
    p7zip
    dmg2img
    # nix ===
    nil
    nixfmt
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
    cataclysm-dda
    lutris
    heroic
    wineWowPackages.stagingFull
    winetricks
    protonup-ng
    protontricks

    # === custom packages ===
    #deity
  ];
}
