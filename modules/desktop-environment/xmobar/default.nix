{ config, pkgs, themes, ... }:
{
  imports = [
    ./rust-blocks
  ];

  home.packages = [ 
    (pkgs.haskellPackages.callPackage ./my-xmobar.nix { inherit themes; })
  ];

  # systemd.user.services."rust-blocks" = {
  #   Description = "Xmobar blocks with asyn rust";
  #   ServiceConfig = {
  #     ExecStart = "${pkgs.rust-blocks}/bin/rust-blocks";
  #   };
  #   WantedBy = [ "default.target" ];
  # };
}
