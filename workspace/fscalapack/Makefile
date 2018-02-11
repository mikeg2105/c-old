
# GNUmakefile to build and execute ACML example programs

# Fortran compiler and flags (modify if necessary)
#
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

RESULTS := $(FEXE:.exe=.res)

all: $(FEXE)

# Rule to compile a Fortran file
$(FOBJ): $(TMPBUILDDIR)/%.$(obj) : source/%.f
	@mkdir -p $(TMPBUILDDIR)
	$(F77) -c $(FFLAGS) $^ -o $@

# Rule to link a Fortran example program
$(FEXE): $(TMPBUILDDIR)/%.exe : $(TMPBUILDDIR)/%.$(obj)
	@echo "Linking program $^:"
	$(FLINK) $(FLINKFLAGS) $^ $(ALLLIBS) $(FLINKLIBS) -o $@

# Rule to execute an example program and produce a .res file
$(RESULTS): $(TMPBUILDDIR)/%.res : $(TMPBUILDDIR)/%.exe
	@echo ""
	@echo "Running program $^:"
	$(MPIDIR)/bin/mpirun -np 6 $^ > $@ 2>&1
#       Show the results
	@cat $@

clean:
	rm -f $(FEXE) $(FOBJ) $(RESULTS)
