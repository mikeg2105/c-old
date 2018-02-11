# MPI Makefile for intrompi examples

.SUFFIXES: .f90 .f .o


MPI_HOME    = /usr/local/mpich-gm2_PGI
MPI_INCLUDE =  $(MPI_HOME)/include 

ARCH        = 
COMM        =

LIB_PATH    =
LIBS        =

#
# C compiler and options
#
CC          = ${MPI_HOME}/bin/mpicc 
CLINKER     = ${CC}
COPTFLAGS    = -O -fast

F90         = ${MPI_HOME}/bin/mpif90
FLINKER     = $(F90)
FOPTFLAGS    = -O3 -fast

LINKER=$(CLINKER)
OPTFLAGS=$(COPTFLAGS)

#
# Object files
#
OBJ=    hello_mpi.o
#	dfill.o \
#	domove.o \
#        dscal.o \
#
#	velavg.o

#
# Compile
#
ex1:	$(OBJ)
	$(CC) -o $@ $(OBJ) $(LIBS)

.c.o:
	$(CC) -c $<

#
# Clean out object files and the executable.
#
clean:
	rm *.o ex1
