![cool headed](./cool.png)

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


so the spells are:
```
nix-build .
ls result/sd-image/
zstdcat result/sd-image/nixos-image-sd-card-25.11pre891648.f6b44b240152-aarch64-linux.img.zst > /dev/sda
```

assuming your sdcard is on sda.
you can find that out with lsblk and lsusb

NB: no dd madness, cat is faster! dd set's a fixed block size, cat just searches a good one.



okay okay we got that,
I just shoved it into the raspberry pi, turned it on, prayed a bit,
and then opene my router (ip adddr and then the first or last address of range), 
it said a "nixos" had appeared.

guess that's us?

it has ip 192.168.0.148
so I ssh into that and it let me in.
success!

so now I want to switch a new config into it.

TBD

