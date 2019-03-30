with import ./. {};

hpkgs.shellFor {
  packages = p: [ hpkgs.linz ];
  nativeBuildInputs = [ hpkgs.cabal-install ];
}
