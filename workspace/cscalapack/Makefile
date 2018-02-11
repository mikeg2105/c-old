# MPI Makefile for intrompi examples





ARCH        = 
COMM        =

LIB_PATH    =
LIBS        =

MPI_HOME = /usr/local/mpich-gm2_PGI
MPI_INCLUDE = $(MPI_HOME)/include
MPIDIR=$(MPI_HOME)
TMP=jobs

# Location of ACML installation
ACMLDIR := /usr/local/acml-pg2.6.0/pgi64
SCAACMLDIR := /usr/local/acml2.5.0/pgi64
F77 := ${MPI_HOME}/bin/mpif77
FLINK := ${MPI_HOME}/bin/mpif77



FFLAGS := -tp=k8-64 -Mcache_align
FLINKFLAGS := 
FLINKLIBS :=

obj := o 
.SUFFIXES: .f .$(obj) .exe .res

ifndef MPIDIR
  $(error Environment variable MPIDIR is not set)
endif

LIBSPACK := $(SCAACMLDIR)/lib/libscalapack.a
LIBBLACS := $(SCAACMLDIR)/lib/libblacsF77init.a $(SCAACMLDIR)/lib/libblacs.a \
            $(SCAACMLDIR)/lib/libblacsF77init.a
LIBACML  := $(ACMLDIR)/lib/libacml.a
LIBMPI   := -L$(MPIDIR)/lib -lmpich

# A directory with write access for objects and executables
TMPBUILDDIR := $(TMP)

ALLLIBS  := -lpgc $(LIBSPACK) $(LIBBLACS) -L/usr/local/acml-pg2.6.0/pgi64/lib  -lacml $(LIBMPI)

# Source code of Fortran example programs
FSOURCE := source/example1.f
FOBJ := $(TMPBUILDDIR)/$(notdir $(FSOURCE:.f=.$(obj)))
FEXE := $(TMPBUILDDIR)/$(notdir $(FSOURCE:.f=.exe))



#
# C compiler and options
#
CC          = ${MPI_HOME}/bin/mpicc 
CLINKER     = ${CC}
COPTFLAGS    = -O -fast


LINKER=$(CLINKER)
OPTFLAGS=$(COPTFLAGS)

#
# Object files
#
OBJ=    cscalapack.o
#	dfill.o \
#	domove.o \
#        dscal.o \
#
#	velavg.o

all: cscalapack

#
# Compile
#
cscalapack:	$(OBJ)
	$(CC) -o $@ $(OBJ) $(ALLLIBS)

.c.o:
	$(CC) -c $<

#
# Clean out object files and the executable.
#
clean:
	rm *.o cscalapack
