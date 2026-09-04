{
  config,
  pkgs,
  lib,
  themes,
  anyrun,
  system,
  ...
}:

# Example /use/share/wayland-sessions/hyprland.desktop:
# [Desktop Entry]
# Name=Hyprland
# Exec=/home/iliayar/.nix-profile/bin/zsh -c 'nixGL start-hyprland'
# Type=Application

with lib;

let
  cfg = config.custom.de.wayland.hyprland;

  tools = {
    select_audio_output = pkgs.writeShellScriptBin "select_audio_output" ''
      devices=$(pw-dump | jq 'map(select(.info.props."device.class" == "sound")) | map(select(.info.props."media.class" == "Audio/Sink")) | map({"name": .info.props."node.nick", "id": .id})')

      selected=$(echo $devices | jq '.[].name' | vicinae dmenu)
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
    swayidle -w timeout ${toString cfg.lock.timeout} '${my-lock}/bin/lock' \
                timeout ${toString cfg.lock.suspendTimeout} 'systemctl suspend' \
                before-sleep '${my-lock}/bin/lock' &
  '';

  last-screenshot = pkgs.writeShellScriptBin "last-screenshot" ''
    set -e

    SCREENSHOTS_DIR=$HOME/Pictures/screenshots
    FILENAME=$(ls "$SCREENSHOTS_DIR/" | sort -r | head -n 1)

    echo "$SCREENSHOTS_DIR/$FILENAME"
  '';
in
{
  options = {
    custom.de.wayland.hyprland = {
      enable = mkOption { default = false; };
      termCmd = mkOption { default = "wezterm"; };
      kbOptions = mkOption { default = "grp:toggle,caps:escape_shifted_capslock"; };
      lock = {
        enable = mkOption { default = false; };
        timeout = mkOption { default = 300; };
        suspendTimeout = mkOption { default = cfg.lock.timeout + 300; };
      };
      startupExtra = mkOption {
        default = [ ];
        type = types.listOf types.str;
      };
      portals.enable = mkOption { default = false; };
      cursor.hyprcursor = mkOption { default = null; };
      cursor.xcursor = mkOption { default = null; };

      flameshot = {
        enable = mkOption { default = false; };
        leftmostMonitor = mkOption { default = "DP-1"; };
        width = mkOption { default = "(monitor_w*2)"; };
        height = mkOption { default = "(monitor_h)"; };
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home.packages = with pkgs; [
        bemenu
        j4-dmenu-desktop
        xrandr
        waypaper
        awww
        grim
        slurp
        wl-clipboard
        satty
        playerctl
        pyprland
        nwg-displays
        wlr-randr
        wdisplays
        wlr-layout-ui
        brightnessctl
        swayidle
        hyprshade
        last-screenshot

        vimix-cursors
        bibata-cursors
      ];

      programs.vicinae = {
        enable = true;
        systemd = {
          enable = true;
          autoStart = true;
        };
      };

      services.flameshot = {
        enable = cfg.flameshot.enable;
        settings = {
          General = {
            contrastOpacity = 120;
            savePath = "Pictures/screenshots";
            saveAfterCopy = true;
          };
        };
      };

      wayland.windowManager.hyprland = {
        enable = true;
        # package = hyprland.packages.${system}.default;
        xwayland = {
          enable = true;
        };
        plugins = with pkgs.hyprlandPlugins; [ hy3 ];

        configType = "lua";
        extraLuaFiles = {
          "nixcfg" =
            let
              startupExtra = foldr (cmd: a: "${a}\"${cmd}\",") "" cfg.startupExtra;
            in
            ''
              local M = {}
              M.mainMod = "SUPER"

              M.theme = {
                font = "${themes.font}",
                colActiveBg   = "rgba(${themes.hex.brightBlack}ff)",
                colActiveFg   = "rgba(${themes.hex.foreground}ff)",
                colFocusedBg  = "rgba(${themes.hex.black}ff)",
                colFocusedFg  = "rgba(${themes.hex.foreground}ff)",
                colInactiveBg = "rgba(${themes.hex.black}ff)",
                colInactiveFg = "rgba(${themes.hex.foreground}ff)",
              }

              M.hyprcursor = ${if cfg.cursor.hyprcursor != null then "\"${cfg.cursor.hyprcursor}\"" else "nil"}
              M.xcursor = ${if cfg.cursor.xcursor != null then "\"${cfg.cursor.xcursor}\"" else "nil"}

              M.kbOptions = "${cfg.kbOptions}"

              M.startupExtra = {${startupExtra}}
              M.autoLock = ${if cfg.lock.enable then "\"${my-autolock}/bin/my-autolock\"" else "nil"}
              M.lock = ${if cfg.lock.enable then "\"${my-lock}/bin/lock\"" else "nil"}

              M.browser = "${config.custom.de.browsers.default}"
              M.terminal = "${cfg.termCmd}"

              -- FIXME: Move to config
              M.player = "spotify"

              M.flameshot = ${
                if cfg.flameshot.enable then ''
                    {
                        size = "${cfg.flameshot.width} ${cfg.flameshot.height}"
                    }
                '' else "nil"
              }

              M.screenshot = "${my-screenshot}/bin/my-screenshot"

              return M
            '';
          "config" = {
            content = ./config.lua;
            autoLoad = true;
          };
        };
        extraConfig = ''
        '';
      };

      xdg.configFile."satty/config.toml".text = ''
        [general]
        early-exit = true
        annotation-size-factor = 1
      '';

      xdg.configFile."pypr/config.toml".text = ''
        [pyprland]
        plugins = ["scratchpads", "monitors", "shortcuts_menu"]

        [shortcuts_menu]
        engine = "vicinae"

        [shortcuts_menu.entries]
        # TODO: Move from top one

        "[TF] Toggle Float" = "hyprctl dispatch workspaceopt allfloat"
        # In vicinae
        # "[E] Emoji" = "${pkgs.bemoji}/bin/bemoji"
        "[AO] select Audio Output" = "${tools.select_audio_output}/bin/select_audio_output"

        [scratchpads.term-quake]
        # command = "wezterm start --class term-quake"
        # class = "term-quake"
        command = "ghostty --class=local.iliayar.term-quake"
        class = "local.iliayar.term-quake"
        position = "0% 0%"
        size = "100% 50%"
        animation = ""
        lazy = false

        [monitors]
        hotplug_command = "wlrlui -m"
        # [monitors.placement."Samsung"]
        # rightOf = "California Institute"

        # [monitors.placement."Dell"]
        # topOf = "California Institute"

        # [monitors.placement."Lenovo"]
        # topOf = "California Institute"

        # [monitors.placement."Acer"]
        # rightOf = "California Institute"
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
