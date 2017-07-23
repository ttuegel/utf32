{ mkDerivation, base, hedgehog, mtl, parsec, stdenv, text
, transformers, vector
}:
mkDerivation {
  pname = "utf32";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base mtl parsec transformers vector ];
  testHaskellDepends = [ base hedgehog text ];
  homepage = "https://github.com/ttuegel/utf32#readme";
  description = "UTF-32-encoded packed string type";
  license = stdenv.lib.licenses.unfree;
}
