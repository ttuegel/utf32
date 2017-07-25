{ mkDerivation, base, cpu, hedgehog, inline-c, mtl, parsec
, semigroups, stdenv, text, transformers, vector
}:
mkDerivation {
  pname = "utf32";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base cpu mtl parsec semigroups transformers vector
  ];
  testHaskellDepends = [ base hedgehog inline-c text ];
  homepage = "https://github.com/ttuegel/utf32#readme";
  description = "UTF-32-encoded packed string type";
  license = stdenv.lib.licenses.unfree;
}
