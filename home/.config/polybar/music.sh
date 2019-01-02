#!/bin/sh

# NOTE: You need the "playerctl" pachage in order for this to work!!!

exec 2>/dev/null

if [ "$(mpc status | grep -e "playing" -c)" = "1" ]; then
  	title=`exec mpc current`
  echo "$title"
else
  echo "No song currently playing"
fi
