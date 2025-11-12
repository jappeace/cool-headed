
sd-image:
	nix-build . -A sdimage

switch:
	nixos-rebuild switch -f ./default.nix -A switch --target-host root@192.168.0.148

shell:
	ssh root@192.168.0.148
