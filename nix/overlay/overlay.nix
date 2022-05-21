{}: self: super:
{
  dwm = super.dwm.overrideAttrs (oldAttrs: rec {
    src = builtins.fetchGit https://github.com/hydra989/dwm;
  });
}
