#!/bin/bash
#
# 

# Set the name of the job (optional) 
#$ -N mpi_systolic_soak
#
# parallel environment request - Edit the following line to change the number of processors that you wish to use
#$ -pe mpich-gm 2-64 
#
# You must request the parallel queue as below along with request the mpich-gm parallel environment
#$ -q parallel.q
#
# MPIR_HOME from submitting environment
#$ -v MPIR_HOME=/usr/local/mpich-gm2_PGI
# ---------------------------

echo "Got $NSLOTS slots."

# enables $TMPDIR/rsh to catch rsh calls if available
set path=($TMPDIR $path)

# Edit the following line if you wish to run a different binary
( echo 100 ; echo 100 ) | $MPIR_HOME/bin/mpirun  -v -np $NSLOTS -machinefile $TMPDIR/machines  ~/courses/intrompi/examples/systolic
