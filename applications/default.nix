{ pkgs, ... }: {
  imports =
    [
      ./alacritty
      ./bspwm
      ./eww
      ./neovim
      ./picom
      ./qutebrowser
      ./rofi
      ./sxhkd
      ./zsh
    ];

  home.packages = with pkgs; [
      # === gui ===
      firefox
      xfce.thunar
      xfce.thunar-archive-plugin
      scrot
      feh
      keepassxc # for old passwords
      bitwarden # migrated to this
      zathura
      pywal
      arandr
      brightnessctl
			discord betterdiscordctl
      github-desktop
			

      # === media ===
      calibre
      mpv
      kodi
      torrential
      popcorntime
      tidal-hifi


      # === dev tools ===
      gh
      virt-manager
      docker
      nixpkgs-review
      vscodium-fhs
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
      calcurse


      # === games ===
      dwarf-fortress
      cataclysm-dda
      heroic
      lutris
      wineWowPackages.stagingFull
      protonup-ng
      protontricks


      # === dependencies ===
      ghostscript
      bc


      # === custom packages ===
      deity
  ];
}
