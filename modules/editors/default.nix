{ config, pkgs, secrets, ... }:

{
  imports = [
    ./neovim
    ./emacs
    ./vscode
  ];

  home.packages = with pkgs; [
    hunspell
    hunspellDicts.en_US
    hunspellDicts.ru_RU
  ];
}
