#!/bin/sh
#make include file for building ising model on iceberg
# Standard defines:
MPI_HOME="/usr/local/Cluster-Apps/mpich-1.2.5..10-pgi"
MPI_INCLUDE="$MPI_HOME/include"
CC=$MPI_HOME"/bin/mpiCC"

LD=$MPI_HOME"/bin/mpiCC"

oDir="."
Bin="."
LIBDIRS="-L../lib"
incDirs="" 
LD_FLAGS="-g"

LIBS="-lm" 
#C_FLAGS	=	-g -DCWDEBUG
C_FLAGS="-O -fast"


MYLIBS=""
export PATH=$PATH":/usr/local/Cluster-Apps/pgi-5.1/linux86/5.1/bin"
export LM_LICENSE_FILE="/usr/local/Cluster-Apps/pgi-5.1/license.dat"
cd proj/cppdev/workspace/matdiag_mpi
$CC $C_FLAGS $incDirs $LINKFLAGS -o $1 -c $2

