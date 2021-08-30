{ config, pkgs, zsh-wakatime, ... }:

{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;

    plugins = [
        {
            name = "zsh-wakatime";
            src = zsh-wakatime;
        }
    ];

    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"
      ];

      custom = "${./.oh-my-zsh/themes}";
      theme = "l";
    };

    initExtra = ''
        export PATH=$PATH:${pkgs.wakatime-cli}/bin
    '';
  };
}
