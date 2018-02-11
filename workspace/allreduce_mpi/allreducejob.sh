#!/bin/sh
#$ -cwd
#$ -pe mpich-gm 4 
#$ -q parallel.q
#$ -N allreduce

# SGE_HOME from submitting environment
#$ -v SGE_HOME=/usr/local/sge6.0

$SGE_HOME/mpi/myrinet/sge_mpirun ./allreduce
