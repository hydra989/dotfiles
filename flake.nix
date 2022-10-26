{
  description = "hhydraa's nixos configuration";

  inputs = {
    # nixpgs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";

    # home-manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {nixpkgs, nixpkgs-master, home-manager, ... }@inputs: rec {
    # general setup
    system = "x86_64-linux";
    config.allowUnfree = true;
    # allow for packages from the master branch of nixpkgs

    overlays = {
      default = import ./nix/overlay.nix { inherit inputs; };
    };
      
    legacyPackages = nixpkgs.lib.genAttrs [ "x86_64-linux" ] (system:
      import inputs.nixpkgs {
        inherit system;
        overlays = builtins.attrValues overlays;
        config.allowUnfree = true;
      }
    );

    # home-manager configurations
    homeConfigurations = {
      "hydra@nightingale" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = { inherit inputs; };
        modules = [ ./nix/home/nightingale.nix ];
      };
    
      "hydra@songbird" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = { inherit inputs; };
        modules = [ ./nix/home/songbird.nix ];
      };
    };

    # machine configurations
    nixosConfigurations = {
      nightingale = nixpkgs.lib.nixosSystem {
        inherit system;
        pkgs = legacyPackages.x86_64-linux;
        specialArgs = { inherit inputs; };
        modules = [
          ./nix/hardware/nightingale.nix
          ./nix/configuration.nix
          ./nix/machines/nightingale.nix
        ];
      };

      songbird = nixpkgs.lib.nixosSystem {
        inherit system;
        pkgs = legacyPackages.x86_64-linux;
        specialArgs = { inherit inputs; };
        modules = [
          ./nix/hardware/songbird.nix
          ./nix/configuration.nix
          ./nix/machines/nightingale.nix
        ];
      };
    };
  };
}
