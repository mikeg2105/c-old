#!/bin/sh
#$ -cwd
#$ -pe openmp 1 
#$ -v OMP_NUM_THREADS=1

./md
