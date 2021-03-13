#!/usr/bin/env bash

eval "export $(egrep -z DBUS_SESSION_BUS_ADDRESS /proc/$(pgrep -u $USER xmonad)/environ)"

out=$(/usr/bin/python "/home/iliayar/bin/$@" 2>&1)

# notify-send "$@" "$out"
