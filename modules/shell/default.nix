{ config, pkgs, ... }:

{
  imports = [
    ./zsh
    # ./bash.nix # Need for run zsh. UPD: No more
  ];
}
