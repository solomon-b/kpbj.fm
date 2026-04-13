#!/usr/bin/env bash
# memory-survey.sh — Sample memory usage of all KPBJ services over time.
#
# Run on the prod VPS for a day or two to capture real peaks before
# setting MemoryMax limits. Writes CSV to stdout.
#
# Usage:
#   ./memory-survey.sh              # sample every 60s until Ctrl-C
#   ./memory-survey.sh 30           # sample every 30s
#   ./memory-survey.sh 60 > mem.csv # save to file
#
# Then pull the CSV locally and look at the max per service:
#   column -t -s, mem.csv | head
#   awk -F, 'NR>1{for(i=2;i<=NF;i++) if($i+0>max[i]){max[i]=$i+0; name[i]=h[i]}} NR==1{for(i=2;i<=NF;i++) h[i]=$i} END{for(i=2;i<=NF;i++) printf "%s: %d MB peak\n", name[i], max[i]/1024/1024}' mem.csv

set -euo pipefail

INTERVAL="${1:-60}"

SERVICES=(
  kpbj-web.service
  postgresql.service
  kpbj-liquidsoap.service
  kpbj-icecast.service
  kpbj-webhook.service
  nginx.service
  loki.service
  grafana.service
  promtail.service
  fail2ban.service
)

# Header
header="timestamp,mem_available_bytes,mem_total_bytes"
for svc in "${SERVICES[@]}"; do
  name="${svc%.service}"
  header+=",${name}_bytes"
done
echo "$header"

while true; do
  ts=$(date -u +%Y-%m-%dT%H:%M:%SZ)

  # System-wide available memory
  mem_avail=$(awk '/MemAvailable/ {print $2 * 1024}' /proc/meminfo)
  mem_total=$(awk '/MemTotal/ {print $2 * 1024}' /proc/meminfo)

  row="${ts},${mem_avail},${mem_total}"

  for svc in "${SERVICES[@]}"; do
    bytes=$(systemctl show "$svc" --property=MemoryCurrent --value 2>/dev/null || echo 0)
    # MemoryCurrent returns "[not set]" if cgroup accounting is off
    if [[ "$bytes" == *"not set"* ]] || [[ -z "$bytes" ]]; then
      bytes=0
    fi
    row+=",${bytes}"
  done

  echo "$row"
  sleep "$INTERVAL"
done
