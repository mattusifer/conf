#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# wait for things to load up
sleep 2

# Launch bar1 and bar2
polybar bar-none  &

echo "Bars launched..."
