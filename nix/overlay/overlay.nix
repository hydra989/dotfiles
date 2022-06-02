{}: self: super:
{
  dwm = super.dwm.overrideAttrs (oldAttrs: rec {
    patches = [
      (super.fetchpatch {
        url = "https://dwm.suckless.org/patches/xresources/dwm-xresources-20210314.diff";
        sha256 = "0g0jgwrssf0jbmaiw8pygzzcf15ldx7gjwiwaq2css2avimfcy1p";
      })
      (super.fetchpatch {
        url = "https://raw.githubusercontent.com/hydra989/dwm/master/hydra.dwm.patch";
        sha256 = "1pq7g9mmjv0qs0z178d85af7mwxhnqdyy923zf93zfqk5d3i63z";
      })
    ];
  });
}
