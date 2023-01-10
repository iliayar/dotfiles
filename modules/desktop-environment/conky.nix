{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.conky;
  iface = "enp0s20f0u2";
  conky-config = { alignment, ... }: ''
    conky.config = {
        alignment = '${alignment}',
        xinerama_head = 1,
        background = false,
        border_width = 1,
        cpu_avg_samples = 2,
        default_color = 'white',
        default_outline_color = 'white',
        default_shade_color = 'white',
        double_buffer = true,
        draw_borders = false,
        draw_graph_borders = true,
        draw_outline = false,
        draw_shades = false,
        extra_newline = false,
        font = '${themes.font}:size=12',
        gap_x = 60,
        gap_y = 60,
        minimum_height = 5,
        minimum_width = 5,
        net_avg_samples = 2,
        no_buffers = true,
        own_window_transparent = false,
        own_window_argb_visual = true,
        own_window_argb_value = 0,
        out_to_x = true,
        own_window = true,
        own_window_class = 'conky',
        own_window_hints = 'below',
        own_window_type = 'override',
        stippled_borders = 0,
        update_interval = 1.0,
        use_spacer = 'none',
        use_xft = true,
    }
  '';
  mkConfig = cfg: text: ''
           ${conky-config cfg}
           
           ${text}
  '';
in
{


  options = {
    custom.de.conky = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.conky
      (pkgs.writeShellScriptBin "run_conky" ''
                             #!${pkgs.bash}/bin/bash
                             ${pkgs.conky}/bin/conky -c ~/.config/conky/conky.conf &
                             ${pkgs.conky}/bin/conky -c ~/.config/conky/conky_date.conf &
    '')
    ];

    xdg.configFile."conky/conky.conf".text = mkConfig {
      alignment = "bottom_left";
    } ''
    conky.text = [[
    ''${color}Highest CPU: ''${goto 300} ''${color}Highest MEM:
    ''${color #e3276b} ''${top name 1}''${top cpu 1} ''${goto 300} ''${color #e3276b} ''${top_mem name 1}''${top_mem mem 1}
    ''${color } ''${top name 2}''${top cpu 2} ''${goto 300} ''${color } ''${top_mem name 2}''${top_mem mem 2}
    ''${color } ''${top name 3}''${top cpu 3} ''${goto 300} ''${color } ''${top_mem name 3}''${top_mem mem 3}
    ''${color } ''${top name 4}''${top cpu 4} ''${goto 300} ''${color } ''${top_mem name 4}''${top_mem mem 4}
    
    ''${color }MEM:  ''${color } $memperc% $mem/$memmax ''${goto 300} ''${color }ROOT:    ''${color }''${fs_free /}/''${fs_size /}
    ''${membar 3,100} ''${goto 300} ''${fs_bar 3,100 /}
    ''${color }SWAP: ''${color }$swapperc% $swap/$swapmax ''${goto 300} ''${color }HOME:  ''${color }''${fs_free /home}/''${fs_size /home}
    ''${swapbar 3,100} ''${goto 300} ''${fs_bar 3,100 /home}
    
    ''${color }NET:
    ''${color}Up: ''${color }''${upspeed ${iface}}k/s ''${goto 300} ''${color}Down: ''${color }''${downspeed ${iface}}k/s''${color}
    ''${upspeedgraph ${iface} 20,130 000000 ffffff} ''${goto 300} ''${downspeedgraph ${iface} 20,130 000000 ffffff}

                                                                                                  ]];
  '';
    xdg.configFile."conky/conky_date.conf".text = mkConfig {
      alignment = "bottom_right";
    } ''
    conky.text = [[
    ''${color }''${time %a, } ''${color }''${time %e %B %G}
    ''${color }''${time %Z,    }''${color }''${time %H:%M:%S}
    ''${color }UpTime: ''${color }$uptime
    ''${color }Kern:''${color }$kernel
    ''${color }CPU:''${color } $cpu% ''${acpitemp}C
    ''${cpugraph 20,130 000000 ffffff}

    ''${color }Load: ''${color }$loadavg
    ''${color }Processes: ''${color }$processes
    ''${color }Running:   ''${color }$running_processes
  ]];
  '';
  };
}
