{
  config,
  pkgs,
  lib,
  themes,
  ...
}:

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
      starship = {
        enable = mkOption {
          default = true;
        };
        transient = mkOption {
          default = true;
        };
      };
      extra = mkOption {
        default = "";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      programs.zsh = {
        enable = true;
        autosuggestion.enable = true;
        syntaxHighlighting.enable = true;

        # FIXME: Remove it
        oh-my-zsh = {
          enable = true;
        };

        shellAliases = {
          magit = "emacsclient -t -e '(magit)'";
        };

        initContent = ''
          ${cfg.extra}
          PATH=$PATH:$HOME/.local/bin
        '';
      };
    })
    (mkIf (cfg.enable && !cfg.starship.enable) {
      programs.zsh = {
        oh-my-zsh = {
          enable = true;
          plugins = [
            "git"
          ];

          custom = "${./.oh-my-zsh/themes}";
          theme = "l";
        };
      };
    })
    (mkIf (cfg.enable && cfg.starship.enable) {
      home.packages = with pkgs; [ jj-starship ];
      programs.starship = {
        enable = true;
        enableZshIntegration = true;
        enableTransience = true;

        settings = {
          add_newline = false;
          format = "$directoryλ ";
          right_format = "\${custom.jj}";

          git_branch.disabled = true;
          git_status.disabled = true;

          directory = {
            style = "blue";
            truncation_length = 1;
          };

          custom.jj = {
            when = "jj-starship detect";
            shell = [
              "jj-starship"
              "--bookmarks-display-limit"
              "1"
            ];
            format = "$output";
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.starship.enable && cfg.starship.transient) {
      programs.zsh.plugins = [
        {
          name = "zsh-transient-prompt";
          file = "transient-prompt.zsh-theme";
          src = pkgs.fetchFromGitHub {
            owner = "olets";
            repo = "zsh-transient-prompt";
            rev = "bdff553570d8ef46f500daf41364bf5c06a11de5";
            sha256 = "sha256-+Tw9TFHtBMrx3JSHmohopZHWeLKUZWzng4NFHCJxLZk=";
          };
        }
      ];
      programs.zsh.initContent = ''
        TRANSIENT_PROMPT_PROMPT='$(starship prompt --terminal-width="$COLUMNS" --keymap="''${KEYMAP:-}" --status="$STARSHIP_CMD_STATUS" --pipestatus="''${STARSHIP_PIPE_STATUS[*]}" --cmd-duration="''${STARSHIP_DURATION:-}" --jobs="$STARSHIP_JOBS_COUNT")'
        TRANSIENT_PROMPT_RPROMPT='$(starship prompt --right --terminal-width="$COLUMNS" --keymap="''${KEYMAP:-}" --status="$STARSHIP_CMD_STATUS" --pipestatus="''${STARSHIP_PIPE_STATUS[*]}" --cmd-duration="''${STARSHIP_DURATION:-}" --jobs="$STARSHIP_JOBS_COUNT")'
        TRANSIENT_PROMPT_TRANSIENT_PROMPT='$(starship prompt --profile transient)'
      '';
      programs.starship = {
        settings = {
          profiles.transient = "$directory\\\$ ";
        };
      };
    })
  ];
}
