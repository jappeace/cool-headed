let
  sources = import ./npins;

in
{
  modulesPath,
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (lib.modules) mkAliasOptionModule;
in
{
  imports = [
    # The generator and hardware configuration
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix")

    #
    # nixpkgs
    #
    {
      nixpkgs = {
        overlays = map import [
          # ../overlays/agenix.nix
        ];

        flake.source = sources.nixpkgs;
      };
    }


    (sources.nixos-hardware + "/raspberry-pi/4")
  ];
  environment.systemPackages = [
    pkgs.man-pages
    pkgs.man-pages-posix
  ];

  #
  # Programs
  #
  programs = {
    vim.enable = true;
    vim.defaultEditor = true;

    git.enable = true;
  };

  # Helps with kitty when ssh from remote
  environment.enableAllTerminfo = true;
  system.stateVersion = "25.05";

  swapDevices = [
    {
      device = "/var/swapfile";
      size = 1024; # MB
    }
  ];

  # Related https://github.com/NixOS/nixpkgs/issues/154163#issuecomment-1350599022
  #
  # modprobe: FATAL: Module sun4i-drm not found in directory /nix/store/gvvwpdckzcr4iamp1iyrqw3nzb7bg6c4-linux-rpi-6.6.51-stable_20241008-modules/lib/modules/6.6.51
  nixpkgs.overlays = [
    (final: prev: {
      makeModulesClosure = x: prev.makeModulesClosure (x // { allowMissing = true; });
    })
  ];

  users.users.root.openssh.authorizedKeys.keys =
    ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILYz4bDXj7t7NQ8QBYKKFBK8myYK6R8/8cybxlMUnIQn hi@jappie.me"]; # TODO

  networking = {
    hostName = "cool-headed";
    networkmanager.enable = lib.mkForce false;

    # To enable roaming https://wiki.archlinux.org/title/Wpa_supplicant#Roaming
    wireless = {
      enable = true;
      userControlled.enable = true;
      scanOnLowSignal = false;
      networks."jappie-hutje".psk = "jappiejappie"; # don't hack me guys, thanks :)
    };
  };
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "prohibit-password";
      PasswordAuthentication = false;
    };
  };


  hardware.bluetooth.enable = true;


}
