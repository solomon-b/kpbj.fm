{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [ "8.8.8.8"
 ];
    defaultGateway = "164.92.96.1";
    defaultGateway6 = {
      address = "";
      interface = "eth0";
    };
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address="164.92.101.240"; prefixLength=19; }
{ address="10.48.0.6"; prefixLength=16; }
        ];
        ipv6.addresses = [
          { address="fe80::5453:71ff:fe84:2895"; prefixLength=64; }
        ];
        ipv4.routes = [ { address = "164.92.96.1"; prefixLength = 32; } ];
        ipv6.routes = [ { address = ""; prefixLength = 128; } ];
      };
            eth1 = {
        ipv4.addresses = [
          { address="10.124.0.7"; prefixLength=20; }
        ];
        ipv6.addresses = [
          { address="fe80::10d4:28ff:fed1:53b3"; prefixLength=64; }
        ];
        };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="56:53:71:84:28:95", NAME="eth0"
    ATTR{address}=="12:d4:28:d1:53:b3", NAME="eth1"
  '';
}
