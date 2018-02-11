#!/bin/sh
#$ -cwd
#$ -j y
#$ -pe mpich-gm 1 
#$ -q parallel.q
#$ -N matdiagjob
#$ -v SGE_HOME=/usr/local/sge6.0

$SGE_HOME/mpi/myrinet/sge_mpirun ../matdiag_mpi 4 
