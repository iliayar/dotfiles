{
  config,
  pkgs,
  lib,
  themes,
  ghostty-newest,
  system,
  ...
}:

with lib;

let
  cfg = config.custom.de.terms;
in
{

  options = {
    custom.de.terms = {
      ghostty = {
        enable = mkOption {
          default = false;
        };
      };
    };
  };

  config = mkIf cfg.ghostty.enable {
    programs.ghostty =
      let
        mod = "alt";
        resize-step = "50";
        styles = pkgs.writeText "config.css" ''
          headerbar {
            margin: 0;
            padding: 0;
            min-height: 12px;
          }

          tabbar tabbox {
            margin: 0;
            padding: 0;
            min-height: 12px;
            background-color: ${themes.background};
            font-family: monospace;
          }

          tabbar tabbox tab {
            margin: 0;
            padding: 0;
            color: ${themes.foreground};
            border-right: 1px solid ${themes.color0};
          }

          tabbar tabbox tab:selected {
            background-color: ${themes.color0};
            color: ${themes.foreground};
          }

          tabbar tabbox tab label {
            font-size: 12px;
          }
        '';
      in
      {
        enable = true;
        enableZshIntegration = true;
        installVimSyntax = true;
        clearDefaultKeybinds = true;

        package =
          if pkgs.stdenv.isDarwin then
            pkgs.writeShellScriptBin "ghostty-fake" ''
              echo "It's fake ghostty"
            ''
          else
            pkgs.ghostty;

        settings = {
          theme = "dark:my-theme,light:my-theme-light";
          window-theme = "dark";
          background-opacity = 0.85;
          font-size = 12;
          font-family = "FiraCode Nerd Font Mono";

        }
        // (
          if pkgs.stdenv.isLinux then
            {
              window-decoration = false;
              gtk-tabs-location = "bottom";
              gtk-wide-tabs = false;
              gtk-custom-css = "${styles}";
            }
          else
            {
              macos-option-as-alt = "left";
            }
        )
        // {
          cursor-style = "block";
          cursor-style-blink = false;
          shell-integration-features = "no-cursor";

          keybind = [
            "${mod}+t=new_tab"
            "${mod}+w=close_surface"
            "${mod}+shift+q=close_surface"
            "${mod}+n=next_tab"
            "${mod}+p=previous_tab"
            "${mod}+shift+n=move_tab:1"
            "${mod}+shift+p=move_tab:-1"

            "${mod}+enter=new_split:right"
            "${mod}+shift+enter=new_split:down"
            "${mod}+f=toggle_split_zoom"
            "${mod}+j=goto_split:down"
            "${mod}+k=goto_split:up"
            "${mod}+h=goto_split:left"
            "${mod}+l=goto_split:right"

            "${mod}+r=activate_key_table:resize"
            "resize/j=resize_split:down,${resize-step}"
            "resize/k=resize_split:up,${resize-step}"
            "resize/h=resize_split:left,${resize-step}"
            "resize/l=resize_split:right,${resize-step}"
            "resize/escape=deactivate_key_table"

            "${mod}+1=goto_tab:1"
            "${mod}+2=goto_tab:2"
            "${mod}+3=goto_tab:3"
            "${mod}+4=goto_tab:4"
            "${mod}+5=goto_tab:5"
            "${mod}+6=goto_tab:6"
            "${mod}+7=goto_tab:7"
            "${mod}+8=goto_tab:8"
            "${mod}+9=goto_tab:9"
            "${mod}+0=goto_tab:10"

            "${mod}+tab=toggle_tab_overview"

            "${mod}+slash=start_search"
          ]
          ++ (
            if pkgs.stdenv.isLinux then
              [
                "ctrl+shift+v=paste_from_clipboard"
                "ctrl+shift+c=copy_to_clipboard"
                "ctrl+shift+plus=increase_font_size:2"
                "ctrl+minus=decrease_font_size:2"
                "ctrl+0=reset_font_size"
              ]
            else
              [
                "cmd+v=paste_from_clipboard"
                "cmd+c=copy_to_clipboard"
                "cmd+plus=increase_font_size:2"
                "cmd+minus=decrease_font_size:2"
                "cmd+0=reset_font_size"

                "option+left=esc:b"
                "option+right=esc:f"
                "global:option+cmd+t=toggle_quick_terminal"
              ]
          );

        };

        themes =
          let
            mk-theme = theme: {
              background = "${theme.hex.background}";
              selection-background = "${themes.hex.foreground}";
              foreground = "${theme.hex.foreground}";
              selection-foreground = "${theme.hex.background}";
              cursor-color = "${theme.hex.cursor}";
              palette = [
                "0=${theme.color0}"
                "1=${theme.color1}"
                "2=${theme.color2}"
                "3=${theme.color3}"
                "4=${theme.color4}"
                "5=${theme.color5}"
                "6=${theme.color6}"
                "7=${theme.color7}"
                "8=${theme.color8}"
                "9=${theme.color9}"
                "10=${theme.color10}"
                "11=${theme.color11}"
                "12=${theme.color12}"
                "13=${theme.color13}"
                "14=${theme.color14}"
                "15=${theme.color15}"
              ];
            };
          in
          {
            my-theme = mk-theme themes.dark;
            my-theme-light = mk-theme themes.light;
          };
      };
  };
}
