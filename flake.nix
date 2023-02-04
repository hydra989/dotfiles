{
  description = "hhydraa's nixos configuration";

  inputs = {
    # nixpgs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable"; # default channel

    # home-manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, home-manager, ... }@inputs: rec {
    # general setup
    system = "x86_64-linux";

    overlays = { default = import ./overlays { inherit inputs; }; };

    legacyPackages = nixpkgs.lib.genAttrs [ "x86_64-linux" ] (system:
      import inputs.nixpkgs {
        inherit system;
        overlays = builtins.attrValues overlays;
        config.allowUnfree = true;
      });

    # home-manager configurations
    homeConfigurations = {
      "hydra" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = { inherit inputs; };
        modules = [ ./home.nix ];
      };
    };

    # machine configurations
    nixosConfigurations = {
      nightingale = nixpkgs.lib.nixosSystem {
        inherit system;
        pkgs = legacyPackages.x86_64-linux;
        specialArgs = { inherit inputs; };
        modules = [
          ./hardware/nightingale.nix
          ./configuration.nix
          ./machines/nightingale.nix
        ];
      };

      songbird = nixpkgs.lib.nixosSystem {
        inherit system;
        pkgs = legacyPackages.x86_64-linux;
        specialArgs = { inherit inputs; };
        modules = [
          ./hardware/songbird.nix
          ./configuration.nix
          ./machines/songbird.nix
        ];
      };
    };
  };
}
