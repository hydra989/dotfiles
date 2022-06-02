{}: self: super:
{
  dwm = super.dwm.overrideAttrs (oldAttrs: rec {
    src = builtins.fetchGit https://github.com/hydra989/dwm;
    patches = [
      (super.fetchpatch {
        url = "https://dwm.suckless.org/patches/xresources/dwm-xresources-20210314.diff";
        sha256 = "0g0jgwrssf0jbmaiw8pygzzcf15ldx7gjwiwaq2css2avimfcy1p";
      })
    ];
  });
}
