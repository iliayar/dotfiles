{ config, lib, pkgs, themes, my-xmonad-contrib, ...}@inputs:

with lib;

let
  cfg = config.custom.de.xmonad;
  xmonad-configured = pkgs.callPackage ./config inputs;
in
{

  options = {
    custom.de.xmonad = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    home.file.".xmonad" = {
      source = xmonad-configured;
      recursive = true;
    };

    home.packages = with pkgs; [
      gotop
      ueberzug
      w3m
      neofetch
      imagemagick

      dzen2 
      gawk
      (writeScriptBin "dzenShowKeymap" ''
      #!${pkgs.bash}/bin/bash

      KEYMAP=$1
      SCREEN=$2
      COLS=4

      background="${themes.background}"
      foreground="${themes.foreground}"
      red="${themes.red}"
      green="${themes.green}"
      yellow="${themes.yellow}"

      INFO=$(awk -v cols=$COLS -v label="$KEYMAP" -v keycol="$yellow" -v labelcol="$red" -v fg="$foreground" \
           '/END/ {exit}
            {
               split($0, keys, ": ");
               key_hint[i++] = sprintf ("^fg(%s)%15.15s ^fg(%s)%-30.30s", keycol, keys[1], fg, keys[2]);
            }
            END {
                printf ("^fg(%s)%s", labelcol, label)
                print ""
                rows = int( ((i+1) / cols) +1)
                for (j=0; j<=i;) {
                    for (k=0; k < rows; k++) {
                         row[k] = row[k] key_hint[j++]
                         }
                     }
                for (k=0; k < rows; k++) {print row[k]}
                print ""
            }')

      echo "$INFO"

      N_LINES=$(wc -l <<< "$INFO")
      (echo "$INFO"; cat) | dzen2 -l $(($N_LINES)) \
        -fn "-*-${themes.font}-*-*-*-*-14-*-*-*-*-*-*-*" \
        -fg $foreground \
        -bg $background \
        -e onstart=uncollapse \
        -xs $SCREEN
    '')  
    ];
  };
}
