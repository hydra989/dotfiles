{ config, lib, pkgs, ... }:
{
  networking = {
    hostName = "songbird";
    interfaces.wlan0.useDHCP = true;
  };

  boot.initrd.kernelModules = [ "amdgpu" ];

  services = {
    # for display brightness keys
    illum.enable = true;

    # amdgpu
    xserver = {
      videoDrivers = [ "amdgpu" ];
      libinput.enable = true;

      # exwm
      windowManager.exwm = {
        enable = true;
        enableDefaultConfig = false;
        loadScript = ''
                   (require 'exwm)
                   (exwm-enable)
        '';
        extraPackages = epkgs: with epkgs; [
          # packages.init.el
	        use-package
	        diminish
	        avy bufler linum-relative
	        magit magit-todos
	        evil evil-collection evil-snipe undo-fu
	        cyberpunk-theme monokai-pro-theme
	        all-the-icons mini-modeline
	        hl-todo dashboard ivy
	        flx ivy-rich all-the-icons-ivy-rich
	        counsel swiper projectile counsel-projectile
	        treemacs treemacs-evil lsp-treemacs treemacs-all-the-icons treemacs-magit

	        # exwm.init.el
	        desktop-environment exwm 

	        # org-anno.init.el
	        fountain-mode writeroom-mode markdown-mode
        
	        # lsp-mode.init.el
	        dtrt-indent tree-sitter tree-sitter-langs
	        lsp-ui lsp-mode company company-box company-quickhelp
	        flycheck yasnippet yaml-mode dockerfile-mode nix-mode
	        go-mode lua-mode elpy lsp-java	

	        # not included
	        vterm        
          multi-vterm
        ];
      };

      # lxqt
      lxqt.enable = true;

      # awesome
      displayManager.awesome.enable = true;
    };

    syncthing = {
      enable = true;
      user = "hydra";
      dataDir = "/home/hydra/syncthing";
      configDir = "/home/hydra/.config/syncthing";
    };

    upower.enable = true;
  };

  hardware.opengl = {
    extraPackages = with pkgs; [
      rocm-opencl-icd
      rocm-opencl-runtime
      amdvlk
    ];
  };
}
