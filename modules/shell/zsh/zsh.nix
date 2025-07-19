{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.shell.zsh;
in
{
  options = {
    custom.shell.zsh = {
      enable = mkOption {
        default = false;
      };
      extra = mkOption {
        default = "";
      };
    };
  };

  config = mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;

      oh-my-zsh = {
        enable = true;
        plugins = [
          "git"
        ];

        custom = "${./.oh-my-zsh/themes}";
        theme = "l";
      };

      shellAliases = {
        magit = "emacsclient -t -e '(magit)'";
      };

      initContent = ''
        ${cfg.extra}
        PATH=$PATH:$HOME/.local/bin
      '';
    };
  };
}
