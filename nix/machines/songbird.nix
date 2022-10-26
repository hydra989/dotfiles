{ inputs, lib, config, pkgs, ...}:
{
  imports = [
    ./configuration.nix
  ];

  networking.hostName = "songbird";
}
