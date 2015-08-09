{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv }:
      mkDerivation {
        pname = "hsTColors";
        version = "0.1.0.0";
        src = ./.;
        buildDepends = [ base ];
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
