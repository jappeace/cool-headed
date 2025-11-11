currently this is a raspberry pi experiment

that can't even build:
```
error: a 'aarch64-linux' with features {} is required to build '/nix/store/2as9b5ggxb9mrwn5dcyhgrx1rb635m14-X-Restart-Triggers-dhcpcd.drv', but I am a 'x86_64-linux' with features {benchmark, big-parallel, kvm, nixos-test}
[ble: exit 1][ble: elapsed 56.833s (CPU 21.0%)] nix-build . 
```


my son, this is the magic spell that gives you rasberry pi and other aarch
buildng magic. use it wisely.                                            
(add to configuration.nix of nixos host)

```
boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
```


Now it builds!
