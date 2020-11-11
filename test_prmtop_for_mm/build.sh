#!/bin/sh

set -e

MM_DIR="/home/osamu/mm.local/mm"
FC=ifort

# Check sanity
if [ "x$MM_DIR" = "x" ]; then exit; fi
if [ "x$FC" = "x" ]; then exit; fi

cp $MM_DIR/gensize.f90 .
$FC gensize.f90 -o gensize
for f in *.prmtop; do
  ./gensize < $f > $f:r.size.inc
done
perl merge_size.pl *.size.inc > size.inc
rm -f *.size.inc
rm -f gensize
\cp ./size.inc $MM_DIR
(cd $MM_DIR; make clean; make)
\cp $MM_DIR/mm.x .

