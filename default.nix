let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/b36dc66bfea6b0a733cf13bed85d80462d39c736.tar.gz";
    sha256 = "1f7vmhdipf0zz19lwx3ni0lmilhnild7r387a04ng92hnc27nnsv";
  };
in
{ pkgs ? import nixpkgs {}
}:
let
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = pkgs.haskellPackages.extend (self: super: {
    linz = self.callCabal2nix "linz" (lib.sourceByRegex ./. [
      "^\\src.*$"
      "^.*\\.cabal$"
      "^Setup.hs$"
      "^LICENSE$"
    ]) {};
  });
in hpkgs.linz // {
  inherit pkgs hpkgs;
}
