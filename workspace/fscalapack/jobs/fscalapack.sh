#!/bin/sh
#$ -cwd
#$ -j y
#$ -pe mpich-gm 6 
#$ -q parallel.q
#$ -N scalapack
#$ -v SGE_HOME=/usr/local/sge6.0

$SGE_HOME/mpi/myrinet/sge_mpirun example1.exe 
