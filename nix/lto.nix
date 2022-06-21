{ config, lib, pkgs, ... }:
let
  tune_canary = import <nixpkgs> {
    localSystem = {
      gcc.arch = "znver3";
      gcc.tune = "znver3";
      system = "x86_64-linux";
    };
  };
  tune_songbird = import <nixpkgs> {
    localSystem = {
      gcc.arch = "znver1";
      gcc.tune = "znver1";
      system = "x86_64-linux";
    };
  };
in
{
  nixpkgs.overlays = [
    (self: super: {
      optimizeWithFlags = pkg: flags:
        pkg.overrideAttrs (old: {
          NIX_CFLAGS_COMPILE = [ (old.NIX_CFLAGS_COMPILE or "") ] ++ flags;
        });
      LTOTune = pkg:
        self.optimizeWithFlags pkg [ "-flto"
                                     "-ftree-vectorize"
                                     # graphite
                                     "-fgraphite-identity"
                                     "-floop-nest-optimize"
                                   ];
      noLTOTune = pkg:
        self.optimizeWithFlags pkg [ "-ftree-vectorize"
                                     # graphite
                                     "-fgraphite-identity"
                                     "-floop-nest-optimize"
                                   ];

      picom = self.LTOTune super.picom;
      polybar = self.LTOTune super.polybar;
      rofi = self.LTOTune super.rofi;
      vim = self.LTOTune super.vim;
      
      # emacs doesn't build with lto
      emacs = self.noLTOTune super.emacs;
      # wine doesn't build with lto
      wine = self.noLTOTune super.wine;
    })
  ];

  # TODO: per-machine import
  environment.systemPackages = with tune_canary; [
    emacs
    picom
    polybar
    rofi
    vim
    wine
  ];
}
