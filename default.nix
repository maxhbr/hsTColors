{ mkDerivation, base, hspec, QuickCheck, stdenv }:
mkDerivation {
  pname = "hsTColors";
  version = "0.2.0.0";
  src = ./.;
  buildDepends = [ base ];
  testDepends = [ base hspec QuickCheck ];
  license = stdenv.lib.licenses.bsd3;
}
