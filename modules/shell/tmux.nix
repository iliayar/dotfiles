{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.custom.shell.tmux;
in
{
  options = {
    custom.shell.tmux = {
      enable = mkOption {
        default = false;
      };

      extraConfig = mkOption {
        default = "";
      };
    };
  };

  config = mkIf cfg.enable {
    programs.tmux = {
        enable = true;
        baseIndex = 1;
        clock24 = true;
        escapeTime = 0;
        keyMode = "vi";
        mouse = true;
        newSession = true;
        terminal = "screen-256color";
        resizeAmount = 20;
        disableConfirmationPrompt = true;

        extraConfig = ''
            unbind-key -a

            ${cfg.extraConfig}

            bind-key C-b send-prefix

            bind-key v split-window -h
            bind-key s split-window -v
            bind-key C-w kill-window
            bind-key C-t new-window
            bind-key l next-window
            bind-key h previous-window
            bind-key Q kill-pane
            bind-key f resize-pane -Z

            bind-key j select-pane -t :.+
            bind-key k select-pane -t :.-

            bind-key Space next-layout

            bind-key 0 select-window -t 0
            bind-key 1 select-window -t 1
            bind-key 2 select-window -t 2
            bind-key 3 select-window -t 3
            bind-key 4 select-window -t 4
            bind-key 5 select-window -t 5
            bind-key 6 select-window -t 6
            bind-key 7 select-window -t 7
            bind-key 8 select-window -t 8
            bind-key 9 select-window -t 9

            bind-key : command-prompt
            bind-key ? list-keys
            bind-key [ copy-mode
            bind-key d detach-client
            bind-key -r C-Up    resize-pane -U 10
            bind-key -r C-Down  resize-pane -D 10
            bind-key -r C-Left  resize-pane -L 10
            bind-key -r C-Right resize-pane -R 10
        '';
    };
  };
}
