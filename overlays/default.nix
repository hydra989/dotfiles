{ inputs, ... }:
let
	additions = final: _prev: import ../packages { pkgs = final; };
in
inputs.nixpkgs.lib.composeManyExtensions [ additions ]
