{ config, pkgs, ... }:

{
  imports = [
    ./zsh.nix
    # ./bash.nix # Need for run zsh. UPD: No more
  ];
}
