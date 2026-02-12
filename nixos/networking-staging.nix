{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [
      "8.8.8.8"
    ];
    defaultGateway = "146.190.144.1";
    defaultGateway6 = {
      address = "";
      interface = "eth0";
    };
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address = "146.190.147.93"; prefixLength = 20; }
          { address = "10.48.0.6"; prefixLength = 16; }
        ];
        ipv6.addresses = [
          { address = "fe80::878:6ff:fe41:2272"; prefixLength = 64; }
        ];
        ipv4.routes = [{ address = "146.190.144.1"; prefixLength = 32; }];
        ipv6.routes = [{ address = ""; prefixLength = 128; }];
      };
      eth1 = {
        ipv4.addresses = [
          { address = "10.124.0.3"; prefixLength = 20; }
        ];
        ipv6.addresses = [
          { address = "fe80::d00d:a0ff:fed9:bcbb"; prefixLength = 64; }
        ];
      };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="0a:78:06:41:22:72", NAME="eth0"
    ATTR{address}=="d2:0d:a0:d9:bc:bb", NAME="eth1"
  '';
}
