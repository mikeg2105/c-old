#!/bin/sh
#make include file for building ising model on iceberg
# Standard defines:
#make include file for building ising model on iceberg
# Standard defines:
MPI_HOME="/usr/local/Cluster-Apps/mpich-1.2.5..10-pgi"
MPI_INCLUDE="$MPI_HOME/include"
CC=$MPI_HOME"/bin/mpiCC"

LD=$MPI_HOME"/bin/mpiCC"

oDir="."
Bin="."
LIBDIRS="-L../lib -L../utils"
incDirs="" 
LD_FLAGS="-g"

LIBS="-lm" 
#C_FLAGS	=	-g -DCWDEBUG
C_FLAGS="-O -fast"


MYLIBS="../utils/vec.o ../utils/mat.o"
export PATH="/usr/local/Cluster-Apps/Modules//bin:/usr/kerberos/bin:/usr/X11R6/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/globus2/bin:.:/usr/local/Cluster-Apps/cluster-tools-0.9/bin:/usr/local/Cluster-Apps/cluster-tools-0.9/sbin:/usr/local/Cluster-Apps/pbs/bin:/usr/local/Cluster-Apps/pbs/sbin:/usr/local/Cluster-Apps/compiler70/ia32/bin/:/usr/local/Cluster-Apps/gm-2.0.8/bin/:/usr/local/vdt/globus/bin:/usr/local/vdt/globus/sbin:/usr/local/Cluster-Apps/pgi-5.1/linux86/5.1/bin:/usr/local/Cluster-Apps/mpich-1.2.5..10-pgi/bin/:/home/data01_a/ngs0244/tools/scilab/scilab-3.1.1/bin:/home/data01_a/ngs0244/bin"
export PATH=$PATH":/usr/local/Cluster-Apps/pgi-5.1/linux86/5.1/bin"
export LM_LICENSE_FILE="/usr/local/Cluster-Apps/pgi-5.1/license.dat"
export LIBPATH="/usr/local/vdt/globus/lib:/usr/lib:/lib:../utils"
export LIBRARY_PATH="/usr/local/Cluster-Apps/mkl61/lib/32"


cd proj/cppdev/workspace/matdiag_mpi
$LD $LIBDIRS $LIBS -o matdiag_mpi $@ $MYLIBS
