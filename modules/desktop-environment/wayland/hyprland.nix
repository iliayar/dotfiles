{ config, pkgs, lib, themes, anyrun, ... }:

with lib;

let
  cfg = config.custom.de.wayland.hyprland;

  hyprland-commands = pkgs.writeShellScriptBin "hyprland-commands" ''
    commands=(
      toggle_float

      gaming
      nogaming

      select_audio_output
      )

      function toggle_float() {
        hyprctl dispatch workspaceopt allfloat 
      }

      function gaming() {
        hyprctl --batch "\
        keyword animations:enabled 0;\
        keyword decoration:drop_shadow 0;\
        keyword decoration:blur 0;\
        keyword general:gaps_in 0;\
        keyword general:gaps_out 0;\
        keyword general:border_size 1;\
        keyword decoration:rounding 0"
      }

      function nogaming() {
        hyprctl reload
      }

      function select_audio_output() {
        devices=$(pw-dump | jq 'map(select(.info.props."device.class" == "sound")) | map(select(.info.props."media.class" == "Audio/Sink")) | map({"name": .info.props."node.nick", "id": .id})')

        selected=$(echo $devices | jq '.[].name' | bemenu)
        selected_id=$(echo $devices | jq "map(select(.name == $selected)) | .[].id")

        wpctl set-default "$selected_id"
      }

      $(echo ''${commands[@]} | tr ' ' '\n' | bemenu)
  '';

  my-screenshot = pkgs.writeShellScriptBin "my-screenshot" ''
    shdir="$HOME/Pictures/screenshots"

    if [ $2 == "f" ]; then
       filename="$shdir/$(date +'%Y-%m-%d-%H%M%S')_full_grim.png"
       grim -t png $filename
    else
      reg=$(slurp)
      res=$(echo $reg | cut -d' ' -f2)
      filename="$shdir/$(date +'%Y-%m-%d-%H%M%S')_''${res}_grim.png"
      grim -g "$reg" -t png $filename
    fi

    if [ $1 == "e" ]; then
      cat $filename | swappy -f - -o $filename
    fi

    cat $filename | wl-copy -t "image/png"

    notify-send -i $filename "$filename"
  '';

  secondMon = "DP-5";
in {
  options = {
    custom.de.wayland.hyprland = { enable = mkOption { default = false; }; };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      bemenu
      j4-dmenu-desktop
      hyprland-commands
      xorg.xrandr
      flameshot
      hyprpaper
      grim
      slurp
      wl-clipboard
      swappy
      playerctl
      pyprland
      nwg-displays
      wlr-randr
      wdisplays
    ];

    programs.anyrun = {
      # FIXME: not ready. gtk css is hard
      enable = false;
      config = {
        layer = "overlay";
        plugins = [
          anyrun.packages.${pkgs.system}.applications
        ];
      };

      extraCss = ''
      window {
        background-color: rgba(0, 0, 0, 0);
      }

      '';
    };

    programs.waybar = {
      package = pkgs.waybar.overrideAttrs
        (old: { mesonFlags = old.mesonFlags ++ [ "-Dexperimental=true" ]; });
      enable = true;
      settings = {
        bottom = {
          layer = "top";
          position = "bottom";
          height = 20;
          output = [ secondMon ];
          modules-left = [
            "hyprland/submap"
            "wlr/workspaces"
            "wlr/taskbar"
            "hyprland/window"
          ];
          modules-center = [ "mpris" ];
          modules-right = [
            "network"
            "pulseaudio"
            "disk#ssd"
            "disk#hdd"
            "cpu"
            "memory"
            "temperature"
            "battery"
            "hyprland/language"
            "clock"
            "tray"
          ];

          "clock" = {
            format = " {:%F (%a) %T}";
            interval = 1;
          };

          "hyprland/language" = { format = "{short}"; };

          "hyprland/submap" = { format = "submap: {}"; };

          "wlr/workspaces" = {
            format = "{name}";
            all-outputs = true;
            active-only = false;
            on-click = "activate";
          };

          "wlr/taskbar" = {
            icon-size = 16;
            on-click = "activate";
          };

          "battery" = { format = " {capacity}%"; };

          "temperature" = {
            iterval = 1;
            thermal-zone = 1;
            format = " {temperatureC}°C";
          };

          "memory" = {
            interval = 1;
            format = " {avail:0.3f}G";
          };

          "cpu" = {
            interval = 1;
            format = " {usage}%";
          };

          "network" = {
            interval = 1;
            format = "{ifname}: {bandwidthDownBits} {bandwidthUpBits}";
          };

          "disk#ssd" = {
            # FIXME: Take username from somewhere
            path = "/home/iliayar";
            format = "hdd: {percentage_used}%";
          };

          "disk#hdd" = {
            path = "/";
            format = "ssd: {percentage_used}%";
          };

          "pulseaudio" = {
            format = " {volume}";
            format-muted = "";
          };

          "mpris" = {
            player = "spotify";
            format = "{artist} - {title}";
          };
        };
      };

      style = ''
        * {
          /* `otf-font-awesome` is required to be installed for icons */
          font-family: "FiraCode", FontAwesome;
          font-size: 14px;
          font-weight: bold;
          padding: 0;
          margin: 0;
          border: none;
          min-height: 0;
          border-radius: 0px;
        }

        window#waybar {
          background-color: ${themes.rgba.background 0.5};
          color: ${themes.foreground};
        }

        #workspaces button {
          color: ${themes.foreground};
        }

        #workspaces button.hidden {
          color: ${themes.brightBlack};
        }

        #workspaces button.active {
          box-shadow: inset 0 -3px ${themes.blue};
        }

        #workspaces button.urgent {
          box-shadow: inset 0 -3px ${themes.red};
        }

        button:hover {
          background: inherit;
          box-shadow: inset 0 -3px ${themes.foreground};
        }

        #clock {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }

        #submap {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.red};
        }

        #language {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }

        #battery {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.green};
        }

        #temperature {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.green};
        }

        #temperature.critical {
          box-shadow: inset 0 -3px ${themes.red};
        }

        #memory {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }

        #cpu {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }

        #network {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }

        #disk {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }

        #mpris {
          box-shadow: inset 0 -3px ${themes.foreground};
        }

        #mpris.playing {
          box-shadow: inset 0 -3px ${themes.green};
        }

        #mpris.paused {
          box-shadow: inset 0 -3px ${themes.yellow};
        }

        #mpris.stopped {
          box-shadow: inset 0 -3px ${themes.red};
        }

        #pulseaudio {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.green};
        }

        #pulseaudio.muted {
          box-shadow: inset 0 -3px ${themes.red};
        }

        #tray > .passive {
          -gtk-icon-effect: dim;
        }

        #tray > .needs-attention {
          -gtk-icon-effect: highlight;
          box-shadow: inset 0 -3px ${themes.red};
        }

        #taskbar .active {
          box-shadow: inset 0 -3px ${themes.blue};
        }

        #window {
          margin-left: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }
      '';
    };

    wayland.windowManager.hyprland = {
      enable = true;

      xwayland = {
        enable = true;
        hidpi = true;
      };
      nvidiaPatches = true;

      extraConfig = ''
        $mainMod = SUPER

        bindm = $mainMod, mouse:272, movewindow
        bindm = $mainMod, mouse:273, resizewindow

        bind = $mainMod, Return, exec, wezterm
        bind = $mainMod, D, exec, bemenu-run
        bind = $mainMod SHIFT, D, exec, j4-dmenu-desktop --dmenu=bemenu
        bind = $mainMod SHIFT, Q, killactive
        bind = $mainMod, F, fullscreen, 0

        general {
          layout = master

          no_cursor_warps = true

          gaps_in = 0;
          gaps_out = 0;
          border_size = 1;
        }

        decoration {
          rounding = 0;
          blur = 0;
          drop_shadow = 0;
        }

        animations {
          enabled = 0;
        }

        master {
          no_gaps_when_only = true
        }

        bind = $mainMod, K, layoutmsg, cycleprev
        bind = $mainMod, J, layoutmsg, cyclenext

        bind = $mainMod SHIFT, K, layoutmsg, swapprev
        bind = $mainMod SHIFT, J, layoutmsg, swapnext

        bind = $mainMod SHIFT, Return, layoutmsg, swapwithmaster master
        bind = $mainMod CTRL, Return, layoutmsg, focusmaster

        bind = $mainMod, bracketleft, focusmonitor, l
        bind = $mainMod, bracketright, focusmonitor, r

        bind = $mainMod SHIFT, bracketleft, movewindow, mon:l
        bind = $mainMod SHIFT, bracketright, movewindow, mon:r

        bind = $mainMod, C, exec, hyprland-commands
        bind = $mainMod SHIFT, print, exec, ${my-screenshot}/bin/my-screenshot e f
        bind = SHIFT, print, exec, ${my-screenshot}/bin/my-screenshot n f
        bind = $mainMod, print, exec, ${my-screenshot}/bin/my-screenshot e
        bind = , print, exec, ${my-screenshot}/bin/my-screenshot

        bind = $mainMod, 1, moveworkspacetomonitor, 1 current
        bind = $mainMod, 1, workspace, 1
        bind = $mainMod, 2, moveworkspacetomonitor, 2 current
        bind = $mainMod, 2, workspace, 2
        bind = $mainMod, 3, moveworkspacetomonitor, 3 current
        bind = $mainMod, 3, workspace, 3
        bind = $mainMod, 4, moveworkspacetomonitor, 4 current
        bind = $mainMod, 4, workspace, 4
        bind = $mainMod, 5, moveworkspacetomonitor, 5 current
        bind = $mainMod, 5, workspace, 5
        bind = $mainMod, 6, moveworkspacetomonitor, 6 current
        bind = $mainMod, 6, workspace, 6
        bind = $mainMod, 7, moveworkspacetomonitor, 7 current
        bind = $mainMod, 7, workspace, 7
        bind = $mainMod, 8, moveworkspacetomonitor, 8 current
        bind = $mainMod, 8, workspace, 8
        bind = $mainMod, 9, moveworkspacetomonitor, 9 current
        bind = $mainMod, 9, workspace, 9
        bind = $mainMod, 0, moveworkspacetomonitor, 10 current
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

        # Resize Submap
        bind = $mainMod, R, submap, resize
        submap = resize

        binde=,L,resizeactive, 30 0
        binde=,H,resizeactive, -30 0
        binde=,J,resizeactive, 0 -30
        binde=,K,resizeactive, 0 30

        bind=,escape,submap,reset
        submap = reset

        # Power Management Submap
        bind = $mainMod SHIFT, E, submap, power
        submap = power

        bind=,S,exec,systemctl suspend
        bind=,S,submap,reset

        bind=,escape,submap,reset
        submap = reset

        # Power Management Submap
        bind = $mainMod SHIFT, E, submap, power
        submap = power

        bind=,S,exec,systemctl suspend
        bind=,S,submap,reset

        bind=,R,exec,systemctl reboot
        bind=,R,submap,reset

        bind=SHIFT,S,exec,systemctl poweroff
        bind=SHIFT,S,submap,reset

        bind=,E,exit
        bind=,E,submap,reset

        bind=,escape,submap,reset
        submap = reset

        # Notifications submap
        bind = $mainMod, N, submap, notifications
        submap = notifications

        bind=,a,exec,dunstctl close-all
        bind=,a,submap,reset

        bind=,escape,submap,reset
        submap = reset

        # Groups submap
        bind = $mainMod, G, submap, groups
        submap = groups

        bind=,t,togglegroup
        bind=,u,moveoutofgroup

        bind=SHIFT,j,moveintogroup,d
        bind=SHIFT,k,moveintogroup,u
        bind=SHIFT,l,moveintogroup,r
        bind=SHIFT,h,moveintogroup,l

        bind=,j,changegroupactive,f
        bind=,k,changegroupactive,b

        bind=,escape,submap,reset
        submap = reset

        # Layout submap
        bind = $mainMod, L, submap, layout
        submap = layout

        bind=,j, layoutmsg, orientationnext
        bind=,k, layoutmsg, orientationprev

        bind=,f,workspaceopt, allfloat

        bind=,escape,submap,reset
        submap = reset

        # Scratchpads
        bind = $mainMod, S, submap, scratchpads
        submap = scratchpads

        bind = ,T, exec, pypr toggle term_quake
        bind = ,T, submap, reset
        $term_quake = ^(term_quake)$
        windowrule = workspace special silent,$term_quake
        windowrule = float,$term_quake

        bind = ,N, exec, pypr toggle org_notes
        bind = ,N, submap, reset
        $org_notes = title:^(org_notes)$
        windowrule = workspace special silent,$org_notes
        windowrule = float,$org_notes

        bind = ,O, exec, pypr toggle obsidian
        bind = ,O, submap, reset
        $obsidian = ^(obsidian)$
        windowrule = workspace special silent,$obsidian
        windowrule = float,$obsidian

        bind=,escape,submap,reset
        submap = reset

        bind=,XF86AudioMute, exec, wpctl set-mute '@DEFAULT_SINK@' toggle
        bind=,XF86AudioLowerVolume, exec, wpctl set-volume '@DEFAULT_SINK@' 5%-
        bind=,XF86AudioRaiseVolume, exec, wpctl set-volume '@DEFAULT_SINK@' 5%+

        $player = spotify        
        bind=,XF86AudioPlay, exec, playerctl -p $player play-pause
        bind=,XF86AudioPrev, exec, playerctl -p $player previous
        bind=,XF86AudioNext, exec, playerctl -p $player next

        # monitor = ${secondMon}, preferred, auto, 1
        # monitor = eDP-1, preferred, auto, 1
        source = ~/.config/hypr/monitors.conf

        input {
          repeat_rate = 50
          repeat_delay = 200
          follow_mouse = 2
          kb_layout = us,ru
          kb_options = grp:switch,grp:caps_toggle
        }

        misc {
          groupbar_gradients = false
          mouse_move_focuses_monitor = false
        }

        windowrule = opacity 0.9 0.9, ^(Spotify)$

        exec-once = xrandr --output ${secondMon} --primary
        exec-once = waybar & hyprpaper & pypr
      '';
    };

    xdg.configFile."hypr/hyprpaper.conf" = {
      text = ''
        preload = /home/iliayar/Wallpapers/2moHU6q.jpg
        preload = /home/iliayar/Wallpapers/VCafhDy.jpg
        preload = /home/iliayar/Wallpapers/HtmoocM.jpg

        wallpaper = eDP-1,/home/iliayar/Wallpapers/2moHU6q.jpg
        wallpaper = ${secondMon},/home/iliayar/Wallpapers/VCafhDy.jpg
      '';
    };

    xdg.configFile."hypr/pyprland.json" = {
      text = builtins.toJSON {
        pyprland.plugins = [ "scratchpads" ];
        scratchpads = {
          "term_quake" = {
            command = "wezterm start --class term_quake";
            position = "0% 0%";
            size = "100% 50%";
          };
          "org_notes" = {
            command = "emacs -T org_notes ~/org/Notes.org";
            position = "10% 10%";
            size = "80% 80%";
          };
          "obsidian" = {
            command = "obsidian";
            position = "10% 10%";
            size = "80% 80%";
          };
        };
      };
    };
  };
}
