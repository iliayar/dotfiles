{ config, pkgs, secrets, ... }:

{
  imports = [
    ./neovim
    ./emacs
    ./wakatime.nix
  ];

  home.packages = with pkgs; [
    hunspell
    hunspellDicts.en_US
    hunspellDicts.ru_RU
  ];
}
