#!/usr/bin/env bash
#
# Delete today's break plan so that it builds again.
#
# The plan for a Pacific day is built once, by the first break request of the
# day or the first view of /dashboard/underwriting/plan. Then it is fixed. This
# script deletes today's entries and today's break_plan_days row. The next
# break request or plan page view builds the whole day again from the current
# items and schedule.
#
# Delete both. With only the day row gone, the build tries to insert entries
# that still exist and fails on the primary key.
#
# Side effects, which are fine for testing:
#   - Breaks that already aired today get a new plan too. The plan page can
#     then show tracks for them that differ from what aired. playback_history
#     still holds what aired.
#   - The rotation starts again from yesterday's entries.
#
# This is a testing tool, so it does not accept prod.
#
# Usage: ./scripts/reset-break-plan.sh <dev|staging>

set -euo pipefail

if [ $# -ne 1 ]; then
  echo "Usage: $0 <dev|staging>" >&2
  exit 1
fi

TARGET="$1"

# The range matches Effects.BreakPlan.dayBoundaries: from the 00:30 boundary
# to the next day's 00:00 boundary, in Pacific time.
SQL=$(
  cat <<'SQL'
\set ON_ERROR_STOP 1
BEGIN;
DELETE FROM break_plan_entries
WHERE boundary >  ((now() AT TIME ZONE 'America/Los_Angeles')::date::timestamp AT TIME ZONE 'America/Los_Angeles')
  AND boundary <= (((now() AT TIME ZONE 'America/Los_Angeles')::date + 1)::timestamp AT TIME ZONE 'America/Los_Angeles');
DELETE FROM break_plan_days
WHERE day = (now() AT TIME ZONE 'America/Los_Angeles')::date;
COMMIT;
SQL
)

case "$TARGET" in
  dev)
    echo "Deleting today's break plan on dev..."
    echo "$SQL" | psql -X -h localhost -p 5433 -U postgres -d dev_db
    ;;
  staging)
    echo "Deleting today's break plan on staging..."
    echo "$SQL" | ssh root@ssh.staging.kpbj.fm 'sudo -u postgres psql -X -d kpbj_fm'
    ;;
  *)
    echo "Error: target must be dev or staging, not '$TARGET'" >&2
    exit 1
    ;;
esac

echo "Done. The next break request or plan page view builds today's plan again."
