{
  description = "hhydraa's nixos configuration";

  inputs = {
    # nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable"; # default channel

    # home-manager
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
            ./configuration.nix
            ./machines/nightingale.nix
          ];
        };

        songbird = nixpkgs.lib.nixosSystem {
          inherit system;
          inherit pkgs;
          modules = [
            ./hardware/songbird.nix
            ./configuration.nix
            ./machines/songbird.nix
          ];
        };
      };
    };
}
