{
  description = "hhydraa's nix flake";

  inputs = {
    # nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # home-manager
    home-manager = {
        url = "github:nix-community/home-manager";
        inputs.nixpkgs.follows = "nixpkgs";
    };

    # nur
    nur.url = "github:nix-community/NUR";
  };

  outputs = { nixpkgs, home-manager, nur, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # home-manager configurations
      homeConfigurations.hydra = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
            ./home.nix
            ./applications
        ];
      };

      # machine configurations
      nixosConfigurations = {
        nightingale = nixpkgs.lib.nixosSystem {
            inherit system;
            inherit pkgs;
            modules = [
                ./hardware/nightingale.nix
                ./machines/nightingale.nix
                ./configuration.nix
            ];
        };

        songbird = nixpkgs.lib.nixosSystem {
            inherit system;
            inherit pkgs;
            modules = [
                ./hardware/songbird.nix
                ./machines/songbird.nix
                ./configuration.nix
            ];
        };

        marv = nixpkgs.lib.nixosSystem {
            inherit system;
            inherit pkgs;
            modules = [
                ./hardware/marv.nix
                ./machines/marv.nix
            ];
        };
      };
    };
}
