#Makefile for matdiag_mpi model

##### User configurable options #####
include make_iceberg
### End User configurable options ###



SRCS	=\
	matdiag_mpi.cpp \
	../utils/vec.cpp \
	../utils/mat.cpp \
	$(oDir)/cmatmpi.cpp \
	$(oDir)/cvecmpi.cpp


EXOBJS	=\
	matdiag_mpi.o \
	../utils/vec.o \
	../utils/mat.o \
	$(oDir)/cmatmpi.o \
	$(oDir)/cvecmpi.o


ALLOBJS	=	$(EXOBJS)
ALLBIN	=	$(Bin)/matdiag_mpi
ALLTGT	=	$(Bin)/matdiag_mpi

#@# Targets follow ---------------------------------

##all:	matdiag_mpi

objs:	$(ALLOBJS)

cleanobjs:
	rm -f $(ALLOBJS)

cleanbin:
	rm -f $(ALLBIN)

clean:	cleanobjs cleanbin

cleanall:	cleanobjs cleanbin






#all:matdiag_mpi
	
#clean:
#	-rm $(OBJ) $(Bin)/matdiag_mpi

#@# Dependency rules follow -----------------------------

all:$(EXOBJS)
	$(LD) $(LIBDIRS) $(LIBS) -o $(Bin)/matdiag_mpi $(EXOBJS) $(MYLIBS)

.cpp.o:
	$(CC) $(C_FLAGS) $(incDirs) $(LINKFLAGS) -o $@ -c $<


