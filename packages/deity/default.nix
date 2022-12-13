{ lib
, stdenv
, fetchFromGitHub
, libX11
}:

stdenv.mkDerivation rec {
  pname = "deity";
  version = "20220725.1";

  src = fetchFromGitHub {
    owner = "hhydraa";
    repo = "deity";
    rev = "v${version}";
    sha256 = "Nryq4SpVWqtfQMostbOM2AoKpHJEC2xBlhKlHgOE6B0=";
  };

  buildInputs = [ libX11 ];

  makeFlags = [ "PREFIX=$(out)" "BINDIR='/bin'" ];

  meta = with lib; {
    description = "";
    homepage = "";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
