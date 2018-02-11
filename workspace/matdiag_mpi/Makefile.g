#Makefile for matdiag_mpi model

##### User configurable options #####
include make_iceberg
### End User configurable options ###



SRCS	=\
	$(oDir)/matdiag_mpi.cpp \
	../utils/vec.cpp \
	../utils/mat.cpp \
	$(oDir)/cmatmpi.cpp

	


EXOBJS	=\
	$(oDir)/matdiag_mpi.o \
	../utils/vec.o \
	../utils/mat.o \
	$(oDir)/cmatmpi.o


ALLOBJS	=	$(EXOBJS)
ALLBIN	=	matdiag_mpi
ALLTGT	=	matdiag_mpi

#@# Targets follow ---------------------------------

##all:	matdiag_mpi

objs:	$(ALLOBJS)



clean:	./ngsclean.sh $(EXOBJS) $(ALLTGT)

cleanall:	cleanobjs cleanbin






#all:matdiag_mpi
	
#clean:
#	-rm $(OBJ) $(Bin)/matdiag_mpi

#@# Dependency rules follow -----------------------------

all:$(EXOBJS)
	./ngsmpild.sh $(EXOBJS)

.cpp.o:
	./ngsmpiCC.sh -o $@ -c $<


