{ config, pkgs, lib, themes, anyrun, system, ... }:

with lib;

let
  cfg = config.custom.de.wayland.hyprland;

  tools = {
    select_audio_output = pkgs.writeShellScriptBin "select_audio_output" ''
      devices=$(pw-dump | jq 'map(select(.info.props."device.class" == "sound")) | map(select(.info.props."media.class" == "Audio/Sink")) | map({"name": .info.props."node.nick", "id": .id})')

      selected=$(echo $devices | jq '.[].name' | bemenu)
      selected_id=$(echo $devices | jq "map(select(.name == $selected)) | .[].id")

      wpctl set-default "$selected_id"
    '';
  };

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
      cat $filename | satty -f - --output-filename $filename
    fi

    cat $filename | wl-copy -t "image/png"

    notify-send -i $filename "$filename"
  '';

  my-lock = pkgs.writeShellScriptBin "lock" ''
    pidof swaylock && exit 0

    swaylock -e -f -c 000000
    # -i ~/Pictures/wallpapers/lock.jpg
  '';

  my-autolock = pkgs.writeShellScriptBin "my-autolock" ''
    swayidle -w timeout 300 '${my-lock}/bin/lock' \
                timeout 600 'systemctl suspend' \
                before-sleep '${my-lock}/bin/lock' &
  '';

  last-screenshot = pkgs.writeShellScriptBin "last-screenshot" ''
    set -e

    SCREENSHOTS_DIR=$HOME/Pictures/screenshots
    FILENAME=$(ls "$SCREENSHOTS_DIR/" | sort -r | head -n 1)

    echo "$SCREENSHOTS_DIR/$FILENAME"
  '';
in {
  options = {
    custom.de.wayland.hyprland = {
      enable = mkOption { default = false; };
      termCmd = mkOption { default = "wezterm"; };
      kbOptions =
        mkOption { default = "grp:toggle,caps:escape_shifted_capslock"; };
      lock.enable = mkOption { default = false; };
      startupExtra = mkOption {
        default = [ ];
        type = types.listOf types.str;
      };
      portals.enable = mkOption { default = false; };
      cursor.hyprcursor = mkOption { default = null; };
      cursor.xcursor = mkOption { default = null; };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home.packages = with pkgs; [
        bemenu
        j4-dmenu-desktop
        xorg.xrandr
        waypaper
        swww
        grim
        slurp
        wl-clipboard
        satty
        playerctl
        pyprland
        nwg-displays
        wlr-randr
        wdisplays
        brightnessctl
        swayidle
        hyprshade
        last-screenshot

        vimix-cursors
        bibata-cursors
      ];

      wayland.windowManager.hyprland = {
        enable = true;
        # package = hyprland.packages.${system}.default;
        xwayland = { enable = true; };

        extraConfig = let
          startupExtra = foldr (cmd: a: ''
            exec-once = ${cmd}
            ${a}
          '') "" cfg.startupExtra;
        in ''
          $mainMod = SUPER

          debug {
           disable_logs = true 
          }

          bindm = $mainMod, mouse:272, movewindow
          bindm = $mainMod, mouse:273, resizewindow

          bind = $mainMod, Return, exec, ${cfg.termCmd}
          bind = $mainMod, D, exec, bemenu-run
          bind = $mainMod SHIFT, D, exec, j4-dmenu-desktop --dmenu=bemenu
          bind = $mainMod SHIFT, Q, killactive
          bind = $mainMod, F, fullscreen, 0

          general {
            layout = master

            gaps_in = 0;
            gaps_out = 0;
            # border_size = 5;

            allow_tearing = true
          }

          master {
            no_gaps_when_only = true
          }

          decoration {
            rounding = false;
            drop_shadow = false;

            blur {
              enabled = false;
            }
          }

          animations {
            enabled = false;
          }

          cursor {
            no_warps = true;
            enable_hyprcursor = false;
            no_hardware_cursors = false;
          }

          opengl {
            force_introspection = false;
          }

          env = WLR_DRM_NO_ATOMIC,1

          env = XCURSOR_SIZE,24
          env = HYPRCURSOR_SIZE,24

          ${if cfg.cursor.hyprcursor != null then ''
            env = HYPRCURSOR_THEME,${cfg.cursor.hyprcursor}
          '' else
            ""}
          ${if cfg.cursor.xcursor != null then ''
            env = XCURSOR_THEME,${cfg.cursor.xcursor}
          '' else
            ""}

          bind = $mainMod, K, layoutmsg, cycleprev
          bind = $mainMod, J, layoutmsg, cyclenext

          bind = $mainMod SHIFT, K, layoutmsg, swapprev
          bind = $mainMod SHIFT, J, layoutmsg, swapnext

          bind = $mainMod SHIFT, Return, layoutmsg, swapwithmaster master
          bind = $mainMod CTRL, Return, layoutmsg, focusmaster

          bind = $mainMod, bracketleft, focusmonitor, +1
          bind = $mainMod, bracketright, focusmonitor, -1

          bind = $mainMod SHIFT, bracketleft, movewindow, mon:l
          bind = $mainMod SHIFT, bracketright, movewindow, mon:r

          bind = $mainMod, C, exec, pypr menu
          bind = $mainMod, T, exec, pypr toggle term-quake
          bind = $mainMod SHIFT, print, exec, ${my-screenshot}/bin/my-screenshot e f
          bind = SHIFT, print, exec, ${my-screenshot}/bin/my-screenshot n f
          bind = $mainMod, print, exec, ${my-screenshot}/bin/my-screenshot e
          bind = , print, exec, ${my-screenshot}/bin/my-screenshot

          bind = $mainMod, 1, focusworkspaceoncurrentmonitor, 1
          bind = $mainMod, 2, focusworkspaceoncurrentmonitor, 2
          bind = $mainMod, 3, focusworkspaceoncurrentmonitor, 3
          bind = $mainMod, 4, focusworkspaceoncurrentmonitor, 4
          bind = $mainMod, 5, focusworkspaceoncurrentmonitor, 5
          bind = $mainMod, 6, focusworkspaceoncurrentmonitor, 6
          bind = $mainMod, 7, focusworkspaceoncurrentmonitor, 7
          bind = $mainMod, 8, focusworkspaceoncurrentmonitor, 8
          bind = $mainMod, 9, focusworkspaceoncurrentmonitor, 9
          bind = $mainMod, 0, focusworkspaceoncurrentmonitor, 10

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

          bind=,R,exec,systemctl reboot
          bind=,R,submap,reset

          bind=SHIFT,S,exec,systemctl poweroff
          bind=SHIFT,S,submap,reset

          ${if cfg.lock.enable then ''
            bind=,L,exec, ${my-lock}/bin/lock
            bind=,L,submap,reset
          '' else
            ""}

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

          bind = ,T, exec, pypr toggle term-quake
          bind = ,T, submap, reset
          # $term_quake = ^(term-quake)$
          # windowrule = workspace special silent,$term_quake
          # windowrule = float,$term_quake

          bind = ,N, exec, pypr toggle org-notes
          bind = ,N, submap, reset
          $org_notes = title:^(org-notes)$
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
          bind=,XF86AudioMicMute, exec, wpctl set-mute '@DEFAULT_SOURCE@' toggle

          bind=,XF86MonBrightnessUp, exec, brightnessctl set +10%
          bind=,XF86MonBrightnessDown, exec, brightnessctl set 10%-

          $player = spotify        
          bind=,XF86AudioPlay, exec, playerctl -p $player play-pause
          bind=,XF86AudioPrev, exec, playerctl -p $player previous
          bind=,XF86AudioNext, exec, playerctl -p $player next

          monitor = ,prefrerred, auto, 1

          input {
            repeat_rate = 50
            repeat_delay = 200
            follow_mouse = 2
            kb_layout = us,ru
            kb_options = ${cfg.kbOptions}

            tablet {
              transform = 0
            }
          }

          misc {
            # groupbar_gradients = false
            mouse_move_focuses_monitor = false
            disable_hyprland_logo = true
            force_default_wallpaper = 0
          }

          windowrule = opacity 0.9 0.9, ^(Spotify)$
          windowrule = opacity 0.9 0.9, ^(VSCodium)$
          windowrule = opacity 0.9 0.9, ^(Code)$
          # windowrule = float, ^(Zoom)$

          exec-once = waypaper --restore
          exec-once = waybar & pypr

          # Extra startup
          ${startupExtra}

          ${if cfg.lock.enable then
            "exec-once = ${my-autolock}/bin/my-autolock"
          else
            ""}
        '';
      };

      xdg.configFile."satty/config.toml".text = ''
        [general]
        early-exit = true
        annotation-size-factor = 1
      '';

      xdg.configFile."hypr/pyprland.toml".text = ''
        [pyprland]
        plugins = ["scratchpads", "monitors", "shortcuts_menu"]

        [shortcuts_menu]
        engine = "bemenu"
        parameters = "-l 10 -p '[prompt]'"

        [shortcuts_menu.entries]
        # TODO: Move from top one

        "[TF] Toggle Float" = "hyprctl dispatch workspaceopt allfloat"
        "[E] Emoji" = "${pkgs.bemoji}/bin/bemoji"
        "[AO] select Audio Output" = "${tools.select_audio_output}/bin/select_audio_output"

        [scratchpads.term-quake]
        command = "wezterm start --class term-quake"
        class = "term-quake"
        position = "0% 0%"
        size = "100% 50%"

        [monitors.placement."Samsung"]
        rightOf = "California Institute"

        [monitors.placement."Dell"]
        topOf = "California Institute"

        [monitors.placement."Lenovo"]
        topOf = "California Institute"

        [monitors.placement."Acer"]
        rightOf = "California Institute"
      '';
    })
    (mkIf (cfg.enable && cfg.portals.enable) {
      xdg = {
        portal = {
          enable = true;
          extraPortals = with pkgs; [
            xdg-desktop-portal-gtk
            xdg-desktop-portal-wlr
            # xdg-desktop-portal-hyprland
          ];
          configPackages = with pkgs; [
            xdg-desktop-portal-wlr
            # xdg-desktop-portal-hyprland 
          ];
          config.common.default = "*";
        };
      };
    })
  ];
}
