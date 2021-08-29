{ ... }:

{
  imports = [
    ./zsh.nix
  ];

  programs.zsh.initExtraFirst = ''
    source ~/.nix-profile/etc/profile.d/nix.sh
  '';
}
