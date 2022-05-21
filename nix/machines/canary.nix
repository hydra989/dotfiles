{ config, pkgs, ... }:
{
  networking = {
    hostName = "canary";
    interfaces.ens18.useDHCP = true;
  };
}
