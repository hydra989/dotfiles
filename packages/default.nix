{ pkgs }: {
  deity = pkgs.callPackage ./deity { };
  i3-gnome = pkgs.callPackage ./i3-gnome { };
}
