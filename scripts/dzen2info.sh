#!/bin/sh
fg="#fcfcfc"
bg="#2d2d2d"
conky -c ~/.xmonad/scripts/conky_dzen2 | dzen2 -p -ta r -e 'button3=' -fn 'Pragmata-8:bold' -fg "$fg" -bg "$bg" -h 24 -w 1280 -x 750
