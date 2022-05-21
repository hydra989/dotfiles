{ config, pkgs, ... }:
{
  networking = {
    hostName = "songbird";
  };

  services.xserver.videoDrivers = [ "amdgpu" ];
  services.flatpak.enable = true;

  environment.systemPackages = with pkgs; [
    # gui
    calibre deluge vlc
  ];
}
