{}: self: super:
{
  dwm = super.dwm.overrideAttrs (oldAttrs: rec {
    src = fetchFromGithub {
      owner = "hydra989";
      repo = "dwm-6.2-fork";
      rev = "25a1954a81c8c389591270ef0c7bd535663bca8d";
      sha256 = "05xjjgyfsacywgriwslhmx0ypb6nyfc3f4fpwp44chzah684l21h";
    };
  });
}
