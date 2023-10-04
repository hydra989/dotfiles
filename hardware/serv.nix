# hardware definition for serv (copied from a config generated
# in a QEMU virtual machine)

{ config, lib, pkgs, modulesPath, ... }:
{
    imports =
        [ (modulesPath + "/profiles/qemu-guest.nix")
        ];

    boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "virtio_pci" "sr_mod" "virtio_blk" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ "kvm-amd" ];
    boot.extraModulePackages = [ ];

    fileSystems."/" =
        { device = "/dev/disk/by-uuid/dd401cc9-58ed-4a4c-b2d4-07a4e33f9726";
          fsType = "ext4";
        };

    swapDevices = [ ];

    networking.useDHCP = lib.mkDefault true;

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
