{ pkgs, ... }:
{
  services.emacs = {
    enable = true;
    package = ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs:
      with epkgs; [
        use-package

	    evil
        evil-org
		evil-collection

		treemacs
		treemacs-evil
		
		magit
		treemacs-magit

		helm

        # languages
        nix-mode
		nix-ts-mode
        # themes
        autothemer
		doom-themes
        cyberpunk-theme
        nordic-night-theme
        catppuccin-theme
      ]
    ));
  };

  programs.emacs = {
    enable = true;
  };
}
