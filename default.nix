{
  sources ? import ./npins,
}:

(import (sources.nixpkgs + "/nixos/lib/eval-config.nix") {
      system = "aarch64-linux";
      modules = [ ./configuration.nix ];
}).config.system.build.sdImage
