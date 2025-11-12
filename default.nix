{
  sources ? import ./npins,
}:
let
  evalled = import (sources.nixpkgs + "/nixos/lib/eval-config.nix") {system = "aarch64-linux"; modules = [ ./configuration.nix ];};
in
{
  sdimage = evalled.config.system.build.sdImage;
  switch = evalled;
}
