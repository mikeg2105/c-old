#!/bin/sh
#$ -cwd
#$ -j y
#$ -pe mpich-gm 4
#$ -q parallel.q
#$ -N isjob
#$ -v SGE_HOME=/usr/local/sge6.0

$SGE_HOME/mpi/myrinet/sge_mpirun ../diffuse_mpi
