{}: self: super:
{
  dwm = super.dwm.overrideAttrs (oldAttrs: rec {
    version = "6.2";
    patches = [
      (super.fetchpatch {
        url = "https://dwm.suckless.org/patches/xresources/dwm-xresources-6.2.diff";
        sha256 = "0z7sbnlh6zhm1fxc62zyx7zmd1ci195gbj7mdsnpcxi1bwaml814";
      })
      (super.fetchpatch {
        url = "https://raw.githubusercontent.com/hydra989/dwm/master/hydra.dwm.patch";
        sha256 = "1pq7g9mmjv0qs0z178d85af7mwxhnqdyy923zf93zfqk5d3i63z";
      })
    ];
  });
}
