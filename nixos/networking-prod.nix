{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [ "8.8.8.8"
 ];
    defaultGateway = "24.199.96.1";
    defaultGateway6 = {
      address = "";
      interface = "eth0";
    };
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address="24.199.102.244"; prefixLength=20; }
{ address="10.48.0.7"; prefixLength=16; }
        ];
        ipv6.addresses = [
          { address="fe80::8463:6dff:fede:33a6"; prefixLength=64; }
        ];
        ipv4.routes = [ { address = "24.199.96.1"; prefixLength = 32; } ];
        ipv6.routes = [ { address = ""; prefixLength = 128; } ];
      };
            eth1 = {
        ipv4.addresses = [
          { address="10.124.0.4"; prefixLength=20; }
        ];
        ipv6.addresses = [
          { address="fe80::d438:44ff:fe5a:9d5c"; prefixLength=64; }
        ];
        };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="86:63:6d:de:33:a6", NAME="eth0"
    ATTR{address}=="d6:38:44:5a:9d:5c", NAME="eth1"
  '';
}
