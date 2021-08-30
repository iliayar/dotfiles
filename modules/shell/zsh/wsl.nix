{ ... }:

{
  imports = [
    ./zsh.nix
  ];

  programs.zsh.initExtraFirst = ''
    source ~/.nix-profile/etc/profile.d/nix.sh
    export DICPATH=$HOME/.nix-profile/share/hunspell
  '';
}
