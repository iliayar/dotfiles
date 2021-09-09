#!/usr/bin/env bash

XPM_ROOT=$(dirname $0)

convert $1 "${XPM_ROOT}/$2.xpm.tmp"

cd "$XPM_ROOT" || exit

convert $2.xpm.tmp -resize 18x18 $2.xpm

rm $2.xpm.tmp
