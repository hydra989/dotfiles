{ lib
, pkgs
, stdenv
, fetchzip
, makeDesktopItem
, unzip
, crispyDoom
}:

let
    doomDesktop = makeDesktopItem {
        name = "DOOM 1.9";
        exec = "crispy-doom -iwad @out@/DOOM.wad";
        icon = "doom 1.9";
        comment = "";
        desktopName = "DOOM 1.9";
        categories = [ "Game" ];
    };

    icons = pkgs.copyPathToStore ./icons;
in
stdenv.mkDerivation (finalAttrs: {
    pname = "DOOM 1.9";
    version = "1.9";
    src = fetchzip {
        url = "https://archive.org/download/2020_03_22_DOOM/DOOM%20WADs/Doom%20%28v1.9%29.zip";
        hash = "sha256-RFTLv0Cg7MmCVH4KtXFSmqZJjzBLZIyohpYTqAdAZpI=";
    };

    nativeBuildInputs = [
        unzip
    ];

    propagatedBuildInputs = [
        crispyDoom
    ];

    dontBuild = true;
    dontConfigure = true;

    installPhase = ''
        install -Dm644 ${icons}/'doom 1.9.png' $out/share/icons/hicolor/32x32/apps/doom.png
        mkdir -p $out/share/applications
        ln -s ${doomDesktop}/share/applications/* $out/share/applications
    '';

    meta = with lib; {
        homepage = "https://archive.org/details/2020_03_22_DOOM";
        description = "";
        maintainers = with maintainers; [ hhydraa ];
    };
})
