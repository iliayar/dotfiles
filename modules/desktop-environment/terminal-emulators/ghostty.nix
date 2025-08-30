{ config, pkgs, lib, themes, ghostty-newest, system, ... }:

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
        # package = ghostty-newest.packages.${system}.default;

        settings = {
          theme = "my-theme";
          background-opacity = 0.85;
          font-size = 12;
          font-family = "FiraCode Nerd Font Mono";

          window-decoration = false;
          gtk-tabs-location = "bottom";
          gtk-wide-tabs = false;
          gtk-custom-css = "${styles}";

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

            "${mod}+ctrl+j=resize_split:down,${resize-step}"
            "${mod}+ctrl+k=resize_split:up,${resize-step}"
            "${mod}+ctrl+h=resize_split:left,${resize-step}"
            "${mod}+ctrl+l=resize_split:right,${resize-step}"

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

            "ctrl+shift+v=paste_from_clipboard"
            "ctrl+shift+c=copy_to_clipboard"

            "ctrl+shift+plus=increase_font_size:2"
            "ctrl+minus=decrease_font_size:2"
            "ctrl+0=reset_font_size"
          ];

        };

        themes = {
          my-theme = {
            background = "${themes.hex.background}";
            selection-background = "${themes.hex.foreground}";
            foreground = "${themes.hex.foreground}";
            selection-foreground = "${themes.hex.background}";
            cursor-color = "${themes.hex.cursor}";
            palette = [
              "0=${themes.color0}"
              "1=${themes.color1}"
              "2=${themes.color2}"
              "3=${themes.color3}"
              "4=${themes.color4}"
              "5=${themes.color5}"
              "6=${themes.color6}"
              "7=${themes.color7}"
              "8=${themes.color8}"
              "9=${themes.color9}"
              "10=${themes.color10}"
              "11=${themes.color11}"
              "12=${themes.color12}"
              "13=${themes.color13}"
              "14=${themes.color14}"
              "15=${themes.color15}"
            ];
          };
        };
      };
  };
}
