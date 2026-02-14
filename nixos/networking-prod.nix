{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [
      "8.8.8.8"
    ];
    defaultGateway = "143.110.144.1";
    defaultGateway6 = {
      address = "";
      interface = "eth0";
    };
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address = "143.110.149.232"; prefixLength = 20; }
          { address = "10.48.0.7"; prefixLength = 16; }
        ];
        ipv6.addresses = [
          { address = "fe80::c0af:13ff:fea7:c4df"; prefixLength = 64; }
        ];
        ipv4.routes = [{ address = "143.110.144.1"; prefixLength = 32; }];
        ipv6.routes = [{ address = ""; prefixLength = 128; }];
      };
      eth1 = {
        ipv4.addresses = [
          { address = "10.124.0.4"; prefixLength = 20; }
        ];
        ipv6.addresses = [
          { address = "fe80::40a5:e8ff:fec1:613d"; prefixLength = 64; }
        ];
      };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="c2:af:13:a7:c4:df", NAME="eth0"
    ATTR{address}=="42:a5:e8:c1:61:3d", NAME="eth1"
  '';
}
