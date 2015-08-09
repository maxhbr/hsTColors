{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "hsTColors";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
