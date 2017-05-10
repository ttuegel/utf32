{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, mtl, parsec, stdenv, transformers
      , vector
      }:
      mkDerivation {
        pname = "utf32";
        version = "0.0.0";
        src = ./.;
        libraryHaskellDepends = [ base mtl parsec transformers vector ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
