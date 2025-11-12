{
  sources ? import ./npins,
  hpkgs ? import ./nix/hpkgs.nix {},
}:
let
  evalled = import (sources.nixpkgs + "/nixos/lib/eval-config.nix") {system = "aarch64-linux"; modules = [ ./configuration.nix ];};
in
{
  sdimage = evalled.config.system.build.sdImage;
  switch = evalled;
  haskell = hpkgs.cool-headed;
}
