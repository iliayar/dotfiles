{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.wayland.hyprland;
in
  {
    options = {
      custom.de.wayland.hyprland = {
        enable = mkOption {
          default = false;
        };
      };
    };

    config = mkIf cfg.enable {
      home.packages = with pkgs; [
        bemenu
        waybar
        wlr-randr
        xorg.xrandr
      ];
      wayland.windowManager.hyprland = {
        enable = true;

        xwayland = {
          enable = true;
          hidpi = true;
        };
        nvidiaPatches = true;

        extraConfig = ''
        $mainMod = SUPER

        bind = $mainMod, Return, exec, alacritty
        bind = $mainMod, D, exec, bemenu-run
        bind = $mainMod SHIFT, Q, killactive
        bind = $mainMod, F, fullscreen, 0

        bind = $mainMod, K, cyclenext
        bind = $mainMod, J, cyclenext, prev

        bind = $mainMod SHIFT, K, swapnext
        bind = $mainMod SHIFT, J, swapnext, prev

        bind = $mainMod, bracketleft, focusmonitor, l
        bind = $mainMod, bracketright, focusmonitor, r

        bind = $mainMod SHIFT, bracketleft, movewindow, mon:l
        bind = $mainMod SHIFT, bracketright, movewindow, mon:r


        bind = $mainMod, 1, moveworkspacetomonitor, 1 current
        bind = $mainMod, 2, moveworkspacetomonitor, 2 current
        bind = $mainMod, 3, moveworkspacetomonitor, 3 current
        bind = $mainMod, 4, moveworkspacetomonitor, 4 current
        bind = $mainMod, 5, moveworkspacetomonitor, 5 current
        bind = $mainMod, 6, moveworkspacetomonitor, 6 current
        bind = $mainMod, 7, moveworkspacetomonitor, 7 current
        bind = $mainMod, 8, moveworkspacetomonitor, 8 current
        bind = $mainMod, 9, moveworkspacetomonitor, 9 current
        bind = $mainMod, 0, moveworkspacetomonitor, 10 current

        bind = $mainMod, 1, workspace, 1
        bind = $mainMod, 2, workspace, 2
        bind = $mainMod, 3, workspace, 3
        bind = $mainMod, 4, workspace, 4
        bind = $mainMod, 5, workspace, 5
        bind = $mainMod, 6, workspace, 6
        bind = $mainMod, 7, workspace, 7
        bind = $mainMod, 8, workspace, 8
        bind = $mainMod, 9, workspace, 9
        bind = $mainMod, 0, workspace, 10

        bind = $mainMod SHIFT, 1, movetoworkspace, 1
        bind = $mainMod SHIFT, 2, movetoworkspace, 2
        bind = $mainMod SHIFT, 3, movetoworkspace, 3
        bind = $mainMod SHIFT, 4, movetoworkspace, 4
        bind = $mainMod SHIFT, 5, movetoworkspace, 5
        bind = $mainMod SHIFT, 6, movetoworkspace, 6
        bind = $mainMod SHIFT, 7, movetoworkspace, 7
        bind = $mainMod SHIFT, 8, movetoworkspace, 8
        bind = $mainMod SHIFT, 9, movetoworkspace, 9
        bind = $mainMod SHIFT, 0, movetoworkspace, 10

        bind = $mainMod CTRL, 1, movetoworkspacesilent, 1
        bind = $mainMod CTRL, 2, movetoworkspacesilent, 2
        bind = $mainMod CTRL, 3, movetoworkspacesilent, 3
        bind = $mainMod CTRL, 4, movetoworkspacesilent, 4
        bind = $mainMod CTRL, 5, movetoworkspacesilent, 5
        bind = $mainMod CTRL, 6, movetoworkspacesilent, 6
        bind = $mainMod CTRL, 7, movetoworkspacesilent, 7
        bind = $mainMod CTRL, 8, movetoworkspacesilent, 8
        bind = $mainMod CTRL, 9, movetoworkspacesilent, 9
        bind = $mainMod CTRL, 0, movetoworkspacesilent, 10

        bind = $mainMod, R, submap, resize

        submap = resize

        binde=,L,resizeactive, 30 0
        binde=,H,resizeactive, -30 0
        binde=,J,resizeactive, 0 -30
        binde=,K,resizeactive, 0 30

        bind=,escape,submap,reset

        submap = reset

        monitor = DP-5, preferred, auto, 1
        monitor = eDP-1, preferred, auto, 1

        input {
          repeat_rate = 50
          repeat_delay = 200
          follow_mouse = 2
          kb_layout = us,ru
          kb_options = grp:switch,grp:caps_toggle,altwin:swap_alt_win
        }

        exec-once = xrandr --output DP-5 --primary
        '';
      };
    };
  }
