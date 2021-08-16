{ config, pkgs, ... }:

{
  imports = map (path: ./misc + "/${path}.nix") [
    "git"
    "ssh"
  ];
}
