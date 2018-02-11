#!/bin/sh
#$ -cwd
#$ -j y
#$ -pe mpich-gm 10 
#$ -q parallel.q
#$ -N allreduce10
#$ -v SGE_HOME=/usr/local/sge6.0

$SGE_HOME/mpi/myrinet/sge_mpirun ../allreduce 
