#!/bin/csh -f
#$ -cwd
#$ -pe smp 4
#$ -q sep.q@sep6
#
setenv g09root /usr/local/g09.b01
setenv GAUSS_EXEDIR /usr/local/g09
setenv GAUSS_SCRDIR /scratch/osamu
setenv GAUSS_ARCHDIR /scratch/osamu
source $g09root/g09/bsd/g09.login
#
  time g09 $1
#
exit 0

