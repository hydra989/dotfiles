{ inputs, config, lib, pkgs, ... }:
{
  networking = {
    useDHCP = false;
    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };
    wireless.iwd.enable = true;
  };

  sound.enable = true;

  hardware = {
    opengl.driSupport = true;
    opengl.driSupport32Bit = true;

    bluetooth.enable = true;

    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };

  hardware.opengl.enable = true;

  nix = {
    optimise.automatic = true;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # steam doesn't have a home-manager module
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };

  # as per zsh home-manager module
  environment = {
    pathsToLink = [ "/share/zsh" ];
    systemPackages = with pkgs; [
      # gui
      firefox xfce.thunar xfce.thunar-archive-plugin
      scrot feh keepassxc zathura picom polybar pywal

      # media
      calibre mpv kodi torrential

      # dev tools
      git gh virt-manager docker nixpkgs-review
      vim qemu qemu-utils

      # kvm-osx
      libguestfs p7zip dmg2img

      # languages
      python3 python3Packages.pip pylint   # python
      gcc gdb clang-tools valgrind # c/c++
      go gopls                     # go
      jdk11                        # java

      # tui
      tty-clock neofetch tor killall
      unzip lm_sensors wrap tmux cmatrix
      comma

      # games
      dwarf-fortress cataclysm-dda heroic minecraft
      lutris wineWowPackages.stagingFull

      # dependencies
      ghostscript bc

      # master branch packages
      master.tidal-hifi
      master.warpd
      master.popcorntime

      # custom packages
      deity
    ];
  };

  virtualisation = {
    # docker
    docker = {
      enable = true;
      enableOnBoot = true;
    };
    # virt-manager
    libvirtd.enable = true;
  };

  # for virt-manager
  programs.dconf.enable = true;

  # for osx-kvm
  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
    options kvm_intel emulate_invalid_guest_state=0
    options kvm ignore_msrs=1
  '';

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      dejavu_fonts
      hack-font
      terminus_font
      font-awesome
      nerdfonts
    ];
  };

  services = {
    blueman.enable = true;

    devmon.enable = true;

    picom = {
      enable = true;
      backend = "glx";
      vSync = true;
    };

    printing.enable = true;

    xserver = {
      enable = true;
      layout = "us";
      displayManager.lightdm.enable = true;

      windowManager.bspwm.enable = true;
    };

    emacs = {
      package = ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs: with epkgs; [
        # packages.init.el
        use-package
        diminish
        avy bufler linum-relative
        magit magit-todos
        evil evil-collection evil-snipe undo-fu evil-mc
        cyberpunk-theme monokai-pro-theme ef-themes
        theme-magic
        all-the-icons
        mini-modeline
        hl-todo
        dashboard
        ivy flx ivy-rich all-the-icons-ivy-rich
        counsel swiper projectile counsel-projectile
        treemacs treemacs-evil lsp-treemacs treemacs-all-the-icons treemacs-magit
        vterm
        multiple-cursors
        toc-org
        which-key

        # lsp
        dtrt-indent
        tree-sitter tree-sitter-langs
        lsp-ui
        lsp-mode
        company company-box company-quickhelp
        flycheck
        yasnippet
        
        # language specific packages
        fountain-mode
        writeroom-mode
        markdown-mode
        yaml-mode
        dockerfile-mode
        nix-mode
        go-mode
        lua-mode
        elpy
        lsp-java
      ]));
      enable = true;
    };
  };

  security = {
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
  };
  
  users.users.hydra = {
    isNormalUser = true;
    extraGroups = [
      "wheel" "networkmanager" "video" "audio" "libvirtd" "docker"
    ];
    initialPassword = "rorschach";
    shell = pkgs.zsh;
  };

  # localization/defaults
  time.timeZone = "America/New_York";

  system.stateVersion = "22.05";
}
