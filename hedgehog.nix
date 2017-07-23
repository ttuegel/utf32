{ mkDerivation, fetchurl, ansi-terminal, async, base, bytestring
, concurrent-output, containers, directory, exceptions
, lifted-async, mmorph, monad-control, mtl, pretty-show, primitive
, random, resourcet, stdenv, stm, template-haskell, text, th-lift
, time, transformers, transformers-base, unix, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "0.5";
  src = fetchurl {
    url = "http://hackage.haskell.org/package/hedgehog-0.5/hedgehog-0.5.tar.gz";
    sha256 = "02dy5fmwmrjgwj6p8rvr53rg362qayavbc184gf2f9q196rgijpk";
  };
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    directory exceptions lifted-async mmorph monad-control mtl
    pretty-show primitive random resourcet stm template-haskell text
    th-lift time transformers transformers-base unix
    wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers pretty-show text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Hedgehog will eat all your bugs";
  license = stdenv.lib.licenses.bsd3;
}
