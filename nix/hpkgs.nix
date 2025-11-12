{ pkgs ? import ./pkgs.nix { }
,
}:
let

  lib = pkgs.haskell.lib;
in
# you can pin a specific ghc version with
  # pkgs.haskell.packages.ghc984 for example.
  # this allows you to create multiple compiler targets via nix.
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    # NB this is a bit silly because nix files are now considered for the build
    # bigger projects should consider putting haskell stuff in a subfolder
    cool-headed =
      lib.overrideCabal (hnew.callCabal2nix "cool-headed" ../. { }) (drv: {
        # reduce build times
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
        doHaddock = false;
        # these two options drastically reduce docker image size
        enableSharedExecutables = false;
        postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";

      });
  };
}
