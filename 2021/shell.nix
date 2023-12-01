{ pkgs ? import <nixpkgs> {}}:

pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = with pkgs; [ (haskellPackages.ghcWithPackages (hs: with hs; [cabal-install])) zlib ];
}
