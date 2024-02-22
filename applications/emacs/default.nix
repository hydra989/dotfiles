{ pkgs, ... }:
{
  services.emacs = {
    enable = true;
    package = ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs:
      with epkgs; [
        use-package
        # languages
        nix-ts-mode
        # themes
        autothemer
        cyberpunk-theme
        nordic-night-theme
      ]
    ));
  };
  programs.emacs = {
    enable = true;
  };
}
