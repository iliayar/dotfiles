{ config, pkgs, secrets, ... }:

{
  imports = [
    ./neovim
    ./emacs
  ];

  home.packages = with pkgs; [
    hunspell
    hunspellDicts.en_US
    hunspellDicts.ru_RU
  ];
}
