#!/bin/bash

# run distcheck (and optionally outdated) on the 
# entire snapshot archive (every 7 days)
# and creates yaml files and time information

DISTCHECK=dose-distcheck
OUTDATED=dose-outdated
RUNOUTDATED=
SNAPSHOT=/srv/debian-snapshot/debian

download () {
  SUITE=$1
  DIR=`pwd`/debian-history-$SUITE-data
  mkdir -p $DIR
  FILES=`ls $SNAPSHOT | awk 'NR % 7 == 0' | tr "\\n" " "`

  tempfile=$(mktemp)
  cat > $tempfile <<EOF
#!/bin/bash
set -x
for day in $FILES; do
  time $DISTCHECK --timers -v deb://$SNAPSHOT/\$day/dists/$SUITE/main/binary-i386/Packages.gz -f > $DIR/\$day.distcheck 2> $DIR/\$day.distcheck.time;
  if $RUNOUTDATED; then
    time $OUTDATED --timers -v deb://$SNAPSHOT/\$day/dists/$SUITE/main/binary-i386/Packages.gz -f > $DIR/\$day.outdated 2> $DIR/\$day.outdated.time;
  fi
  cat $DIR/\$day.time;
done
EOF
  screen -dmS $SUITE sh $tempfile
}

download testing
download unstable
screen -ls

echo "screen -r \$session to resume a session"
