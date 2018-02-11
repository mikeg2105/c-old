#!/bin/sh
#$ -P WhiteRose
#$ -masterq snowdon.q
#$ -cwd

#$ -pe score 5
#$ -l h_cpu=00:05:00
#cd intrompi
scout -wait -F $(HOME)/.score/ndfile.$JOB_ID -e /tmp/scrun.$JOB_ID \
 -nodes=$((NSLOTS-1))x1 ./matdiag_mpi

