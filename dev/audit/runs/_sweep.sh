#!/usr/bin/env bash
set -u
script=$1
# Kill anything on port 4321 (the R/Rscript child)
lsof -ti :4321 2>/dev/null | xargs -r kill -9 2>/dev/null
pkill -9 -f "Rscript.*_runner.R" 2>/dev/null
sleep 1
for i in $(seq 1 10); do
  if ! lsof -ti :4321 >/dev/null 2>&1; then break; fi
  sleep 0.3
done
logfile="dev/audit/runs/$(basename "$script" .R).log"
PORT=4321 nohup Rscript dev/audit/runs/_runner.R "$script" > "$logfile" 2>&1 &
for i in $(seq 1 50); do
  if curl -s --max-time 1 http://127.0.0.1:4321/ >/dev/null 2>&1; then
    echo "READY $script"
    exit 0
  fi
  sleep 0.5
done
echo "FAILED $script  --- log:"
tail -30 "$logfile"
exit 1
