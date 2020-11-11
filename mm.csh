#!/bin/csh -f
#$ -cwd
#$ -q all.q@sha2
#
  time ./debug_pme.x < lattice.dat
#
exit 0

