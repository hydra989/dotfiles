{}: self: super:
{
  dwm = super.dwm.overrideAttrs (oldAttrs: rec {
    version = "6.2";
    patches = [
      (super.fetchpatch {
        url = "https://raw.githubusercontent.com/hydra989/dotfiles/master/dwm/implement-deity-mode.patch";
        sha256 = "";
      })
      (super.fetchpatch {
        url = "https://raw.githubusercontent.com/hydra989/dotfiles/master/dwm/xresources-6.2.patch";
        sha256 = "";
      })
      (super.fetchpatch {
        url = "https://raw.githubusercontent.com/hydra989/dotfiles/master/dwm/header.patch";
        sha256 = "";
      })
    ];
  });
}
