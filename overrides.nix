pkgs: lib: self: super:

{
  hedgehog = self.callPackage ./hedgehog.nix {};
}
