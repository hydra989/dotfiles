{ inputs, ... }:
let
    overlay-master = final: prev: {
      master = inputs.nixpkgs-master.legacyPackages.${prev.system};
    };

    additions = final: _prev: import ./packages { pkgs = final; };
in
inputs.nixpkgs.lib.composeManyExtensions [ additions overlay-master ]
