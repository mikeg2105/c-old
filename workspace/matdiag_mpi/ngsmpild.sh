#!/bin/sh
#make include file for building ising model on iceberg
# Standard defines:
MPI_HOME="/usr/local/mpich-gm2_PGI"
MPI_INCLUDE="$MPI_HOME/include"
CC=${MPI_HOME}"/bin/mpiCC"

LD=${MPI_HOME}"/bin/mpiCC"

oDir="."
Bin="."
LIBDIRS="-L../lib"
incDirs="" 
LD_FLAGS="-g"

LIBS="-lm" 
#C_FLAGS	=	-g -DCWDEBUG
C_FLAGS="-O -fast"


MYLIBS=""


$LD $LIBDIRS $LIBS -o matdiag_mpi $@ $MYLIBS

#echo "Remote linking $1"
#wrexe `wrnn ngs-c1` proj/cppdev/workspace/matdiag_mpi/gridmpild.sh $@

#echo "matdiag_mpi build finished on `wrnn ngs-c1`"
