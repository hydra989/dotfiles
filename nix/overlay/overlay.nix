{}: self: super:
{
  dwm = super.dwm.overrideAttrs (oldAttrs: rec {
    version = "6.2";
    patches = [
      (super.fetchpatch {
        url = "https://raw.githubusercontent.com/hydra989/dotfiles/master/dwm/implement-deity-mode.patch";
        sha256 = "129xj5zbwrrhsqp0izi0603kizk3vmg005f1368zwz2gag6f08a6";
      })
      (super.fetchpatch {
        url = "https://raw.githubusercontent.com/hydra989/dotfiles/master/dwm/xresources-6.2.patch";
        sha256 = "03pm4whzbq8vmqki8lzbhdfbfz2xfrzcgirp0vfa12x5j5iyyhm4";
      })
      (super.fetchpatch {
        url = "https://raw.githubusercontent.com/hydra989/dotfiles/master/dwm/header.patch";
        sha256 = "10m1rpmfcxijmfm7axi779c2whk5rc00sgwvhc9dig69fs8wxqjc";
      })
    ];
  });
}
