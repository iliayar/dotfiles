{ config, pkgs, lib, themes, anyrun, ... }:

with lib;

let
  cfg = config.custom.de.wayland.waybar;
  modules = {
    hyprland-submap = {
      id = "hyprland/submap";
      config = { format = "submap: {}"; };
      style = ''
        #submap {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.red};
        }
      '';
    };
    hyprland-workspaces = {
      id = "hyprland/workspaces";
      config = {
        format = "{name}";
        all-outputs = true;
        active-only = false;
      };
      style = ''
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

      '';
    };
    wlr-taskbar = {
      id = "wlr/taskbar";
      config = {
        icon-size = 16;
        on-click = "activate";
      };
      style = ''
        #taskbar .active {
          box-shadow: inset 0 -3px ${themes.blue};
        }
      '';
    };
    hyprland-window = {
      id = "hyprland/window";
      config = { };
      style = ''
        window#waybar {
          background-color: ${themes.rgba.background 0.5};
          color: ${themes.foreground};
        }
      '';
    };
    mpris = {
      id = "mpris";

      config = {
        player = "spotify";
        format = "{artist} - {title}";
      };

      style = ''
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
      '';
    };
    network = {
      id = "network";
      config = {
        interval = 1;
        format = "{ifname}:  {bandwidthDownBits}  {bandwidthUpBits}";
      };
      style = ''
        #network {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }
      '';
    };
    pulseaudio = {
      id = "pulseaudio";
      config = {
        format = "{format_source}  {volume}";
        format-muted = "{format_source}  ";
        format-source = "";
        format-source-muted = " ";
      };
      style = ''
        #pulseaudio {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.green};
        }

        #pulseaudio.muted {
          box-shadow: inset 0 -3px ${themes.red};
        }
      '';
    };
    idle-inhibitor = {
      id = "idle_inhibitor";
      config = {
        "format" = "{icon} ";
        "format-icons" = {
          "activated" = "";
          "deactivated" = "";
        };
      };
      style = "";
    };
    disk = { path, name }: {
      id = "disk#${name}";
      config = {
        path = path;
        format = " ${name}: {percentage_used}%";
      };
      style = ''
        #disk {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }
      '';
    };
    cpu = {
      id = "cpu";
      config = {
        interval = 1;
        format = " {usage}%";
      };
      style = ''
        #cpu {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }
      '';
    };
    memory = {
      id = "memory";
      config = {
        interval = 1;
        format = " {avail:0.3f}G";
      };
      style = ''
        #memory {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }
      '';
    };
    temperature = {
      id = "temperature";
      config = {
        iterval = 1;
        thermal-zone = 1;
        format = " {temperatureC}°C";
      };
      style = ''
        #temperature {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.green};
        }

        #temperature.critical {
          box-shadow: inset 0 -3px ${themes.red};
        }
      '';
    };
    battery = {
      id = "battery";
      config = { format = " {capacity}%"; };
      style = ''
        #battery {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.green};
        }
      '';
    };
    hyprland-language = {
      id = "hyprland/language";
      config = { format = "{short}"; };
      style = ''
        #language {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }
      '';
    };
    clock = {
      id = "clock";
      config = {
        format = " {:%F (%a) %T}";
        interval = 1;
      };
      style = ''
        #clock {
          margin-right: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }
      '';
    };
    tray = {
      id = "tray";
      config = {

      };
      style = ''
        #tray > .passive {
          -gtk-icon-effect: dim;
        }

        #tray > .needs-attention {
          -gtk-icon-effect: highlight;
          box-shadow: inset 0 -3px ${themes.red};
        }
      '';
    };
  };
in {
  options = {
    custom.de.wayland.waybar = {
      enable = mkOption { default = false; };

      modules = let
        modules-option = mkOption {
          default = (modules: [ ]);
          type = mkOptionType {
            name = "waybar modules";
            merge = (loc: defs:
              foldr ({ value, ... }: a: (ms: (value ms) ++ (a ms))) (ms: [ ])
              defs);
          };
        };
      in {
        left = modules-option;
        center = modules-option;
        right = modules-option;
      };
    };
  };

  config = mkIf cfg.enable {
    programs.waybar = let
      mods-left = cfg.modules.left modules;
      mods-center = cfg.modules.center modules;
      mods-right = cfg.modules.right modules;

      extract-modules-ids = mods: map (m: m.id) mods;
      extract-modules-configs = mods:
        foldr (m: a: { "${m.id}" = m.config; } // a) { } mods;
      extract-modules-style = mods:
        foldr (m: a: ''
          /* ${m.id} */
          ${m.style}

          ${a}
        '') "" mods;
    in {
      enable = true;
      settings = {
        bottom = {
          layer = "top";
          position = "bottom";
          height = 20;
          modules-left = extract-modules-ids mods-left;
          modules-center = extract-modules-ids mods-center;
          modules-right = extract-modules-ids mods-right;
        } // (extract-modules-configs mods-left)
          // (extract-modules-configs mods-center)
          // (extract-modules-configs mods-right);
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

        button:hover {
          background: inherit;
          box-shadow: inset 0 -3px ${themes.foreground};
        }

        #window {
          margin-left: 5px;
          box-shadow: inset 0 -3px ${themes.foreground};
        }

        ${extract-modules-style mods-left}
        ${extract-modules-style mods-center}
        ${extract-modules-style mods-right}
      '';
    };
  };
}
