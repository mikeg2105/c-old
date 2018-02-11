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

$CC $C_FLAGS $incDirs $LINKFLAGS -o $2 -c $4

echo "Tranferring $4"
wrft iceberg $4 ngs-c1 proj/cppdev/workspace/matdiag_mpi/$4
echo "Tranferring $4 complete."
#echo "Remote compiling $4"
#wrexe `wrnn ngs-c1` proj/cppdev/workspace/matdiag_mpi/gridmpicc.sh $2 $4
#echo "Built $2 at `wrnn ngs-c1`"
