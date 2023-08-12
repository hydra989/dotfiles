{ pkgs, ... }: {
  boot.kernelPackages = pkgs.linuxPackages_latest;

  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.1.1u"
  ];

  networking = {
    useDHCP = false;
    networkmanager = {
      enable = true;
    };
  };

  sound = {
    enable = true;
      mediaKeys = {
        enable = true;
        volumeStep = "5%";
      };
  };

  hardware = {
    opengl.enable = true;
    opengl.driSupport = true;
    opengl.driSupport32Bit = true;

    bluetooth.enable = true;

    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };

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

  programs.zsh.enable = true;

  environment = {
    # bootstrap
    systemPackages = with pkgs; [ home-manager git gh networkmanagerapplet i3ipc-glib xfce.xfce4-i3-workspaces-plugin ];

    # as per zsh home-manager module
    pathsToLink = [ "/share/zsh" ];

    # settings for Proton-GE
    sessionVariables = {
      XDG_CACHE_HOME = "\${HOME}/.cache";
      XDG_CONFIG_HOME = "\${HOME}/.config";
      XDG_BIN_HOME = "\${HOME}/.local/bin";
      XDG_DATA_HOME = "\${HOME}/.local/share";
      # Steam needs this to find Proton-GE
      STEAM_EXTRA_COMPAT_TOOLS_PATHS =
        "\${HOME}/.steam/root/compatibilitytools.d";
      # note: this doesn't replace PATH, it just adds this to it
      PATH = [ "\${XDG_BIN_HOME}" ];
    };
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

  # also for virt-manager
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
      material-icons
      material-design-icons
      unifont
    ];
  };

  services = {
    blueman.enable = true;

    devmon.enable = true;

    printing.enable = true;

    xserver = {
      enable = true;
      layout = "us";
      displayManager.lightdm.enable = true;

      desktopManager = {
        xterm.enable = false;
        xfce = {
          enable = true;
          enableXfwm = false;
        };
      };

      windowManager.i3.enable = true;
    };

    emacs = {
      package = ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs:
        with epkgs; [
          use-package
          diminish
          avy
          bufler
          linum-relative
          magit
          magit-todos
          all-the-icons
          mini-modeline
          hl-todo
          dashboard
          ivy
          flx
          ivy-rich
          all-the-icons-ivy-rich
          counsel
          swiper
          projectile
          counsel-projectile
          treemacs
          lsp-treemacs
          treemacs-all-the-icons
          treemacs-magit
          vterm
          multiple-cursors
          which-key
          pdf-tools

          # themes
          cyberpunk-theme
          monokai-pro-theme
          ef-themes
          theme-magic

          # org
          org-superstar
          toc-org

          # latex
          latex-preview-pane

          # evil
          evil
          evil-commentary
          evil-collection
          evil-snipe
          undo-fu
          evil-mc
          evil-org
          treemacs-evil

          # lsp
          tree-sitter
          tree-sitter-langs
          lsp-ui
          lsp-mode
          company
          company-box
          company-quickhelp
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
    extraGroups =
      [ "wheel" "video" "audio" "libvirtd" "docker" ];
    initialPassword = "rorschach";
    shell = pkgs.zsh;
  };

  # localization/defaults
  time.timeZone = "America/New_York";

  system.stateVersion = "22.11";
}
