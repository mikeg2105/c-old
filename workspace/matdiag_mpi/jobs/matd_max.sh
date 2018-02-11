#!/bin/sh
#$ -P WhiteRose
#$ -cwd

#$ -pe mpi_pe 4
#$ -l h_cpu=00:05:00
#cd intrompi
mprun -x sge ../matdiag_mpi 8

