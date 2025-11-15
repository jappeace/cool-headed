let
  sources = import ./npins;

in
{ modulesPath
, pkgs
, lib
, ...
}:
let
  hpkgs = import ./nix/hpkgs.nix { pkgs = pkgs;};
in
{
  imports = [
    # The generator and hardware configuration
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix") # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/installer/sd-card/sd-image-aarch64.nix#L25

    #
    # nixpkgs
    #
    {
      nixpkgs = {
        overlays = map import [
          # ../overlays/agenix.nix
        ];

        # Set NIX_PATH and flake registry at the same time
        # https://github.com/NixOS/nixpkgs/pull/254405
        flake.source = sources.nixpkgs;
      };
    }


    (sources.nixos-hardware + "/raspberry-pi/4")
  ];

  # the serial port that should go to Bluetooth is currently being used as a kernel console, so the Bluetooth stack can’t actually use it.
  # undo https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/installer/sd-card/sd-image-aarch64.nix#L25
  # This replaces the existing console=ttyS0,115200n8 console=ttyAMA0,115200n8 console=tty0 with just console=tty1, so the serial ports are no longer hogged by the kernel console.
  boot.kernelParams = lib.mkForce [
    "console=tty1"
  ];

  # attempt latest kernel to enable blueotooth 6.17.7
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # disable zfs, base profile pulls it in on the sd image:
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/profiles/base.nix#L63
  boot.supportedFilesystems = lib.mkForce [ "vfat" "ext4" ];

  environment.systemPackages = [
    pkgs.man-pages
    pkgs.man-pages-posix
    pkgs.libgpiod
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

  users = { users.root.openssh.authorizedKeys.keys =
    [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILYz4bDXj7t7NQ8QBYKKFBK8myYK6R8/8cybxlMUnIQn hi@jappie.me" ]; # TODO

        extraGroups.coolHeaded = { };
        extraUsers.coolHeaded =
        { description = "CoolHeaded web service";
            group = "coolHeaded";
            useDefaultShell = true;
            isSystemUser = true;
            extraGroups = [ "bluetooth" ];
        };
    };

  networking = {
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


  hardware.bluetooth = {
    powerOnBoot = true;
    enable = true;
  };
  hardware.enableRedistributableFirmware = true;

  hardware.firmware = [
    pkgs.raspberrypiWirelessFirmware
  ];

  # enables dbus
  services.blueman.enable = true;
  services.dbus.enable = true;



  systemd.services.cool-headed=
    {
      description = "Cool headed ble gpio control";
      serviceConfig = {
        Type = "simple";
        ExecStart = "${hpkgs.cool-headed}/bin/exe";

        Restart = "on-failure";
        User = "coolHeaded";
      };
      wantedBy = [ "multi-user.target" ];
    };


}
