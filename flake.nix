{
  description = "hhydraa's nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs_master.url = "github:nixos/nixpkgs/master";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {nixpkgs, nixpkgs_master, home-manager, ... }@inputs: rec {
    system = "x86_64-linux";
    config.allowUnfree = true;
      
    legacyPackages = nixpkgs.lib.genAttrs [ "x86_64-linux" ] (system:
      import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
      }
    );

    homeConfigurations = {
      "hydra@nightingale" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = { inherit inputs; };
        modules = [ ./nix/machines/nightingale.nix ];
      };
    
      "hydra@songbird" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = { inherit inputs; };
        modules = [ ./nix/machines/songbird.nix ];
      };
    };

    nixosConfigurations = {
      nightingale = nixpkgs.lib.nixosSystem {
        inherit system;
        pkgs = legacyPackages.x86_64-linux;
        specialArgs = { inherit inputs; };
        modules = [
          ./nix/hardware/nightingale.nix
          ./nix/configuration.nix
        ];
      };

      songbird = nixpkgs.lib.nixosSystem {
        inherit system;
        pkgs = legacyPackages.x86_64-linux;
        specialArgs = { inherit inputs; };
        modules = [
          ./nix/configuration.nix
          ./nix/hardware/songbird.nix
        ];
      };
    };
  };
}
