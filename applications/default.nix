{ pkgs, ... }: {
  imports = [
    ./alacritty
    ./bspwm
    ./eww
    ./neovim
    ./picom
    ./rofi
    ./sxhkd
    ./zsh
  ];

  home.packages = with pkgs; [
    # === gui ===
    firefox
    chromium
    xfce.thunar
    xfce.thunar-archive-plugin
    scrot
    feh
    keepassxc # for old passwords, i've since
    bitwarden # migrated to this
    zathura
    pywal
    arandr
    brightnessctl
    discord
    betterdiscordctl
    connman-gtk
    obsidian

    # === media ===
    calibre
    mpv
    kodi
    torrential
    popcorntime
    tidal-hifi

    # === dev tools ===
    git
    gh
    virt-manager
    docker
    nixpkgs-review
    # kvm-osx ===
    libguestfs
    p7zip
    dmg2img
    # nix ===
    nil
    nixfmt
    # python
    python3
    python3Packages.pip
    python3Packages.python-lsp-server
    pylint
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
    deity
  ];
}
