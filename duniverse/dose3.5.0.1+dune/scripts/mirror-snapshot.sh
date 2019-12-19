#!/bin/bash

#set -x

dists=${1:-unstable}
years="2011 2012"

for y in $years; do
  for m in {1..12}; do
    if [ $m -lt 10 ] ; then m="0"$m ; fi
    line=$(wget -O - "http://snapshot.debian.org/archive/debian/?year=$y;month=$m" | grep "$y-$m-10" | head -1)
    date=$(echo $line | perl -0ne 'print "$1\n" while (/a href=\"(.*?)\">.*?<\/a>/igs)')
    url=http://snapshot.debian.org/archive/debian/$date/dists/$dists/main/binary-i386/Packages.gz
    mkdir -p archive/$date/dists/$dists/main/binary-i386
    wget -c -O archive/$date/dists/$dists/main/binary-i386/Packages.gz $url

    line=$(wget -O - "http://snapshot.debian.org/archive/debian/?year=$y;month=$m" | grep "$y-$m-25" | head -1)
    date=$(echo $line | perl -0ne 'print "$1\n" while (/a href=\"(.*?)\">.*?<\/a>/igs)')
    url=http://snapshot.debian.org/archive/debian/$date/dists/$dists/main/binary-i386/Packages.gz
    mkdir -p archive/$date/dists/$dists/main/binary-i386
    wget -c -O archive/$date/dists/$dists/main/binary-i386/Packages.gz $url
  done
done
