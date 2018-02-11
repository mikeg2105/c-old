#Makefile for matdiag_mpi model

##### User configurable options #####
include make_iceberg
### End User configurable options ###



SRCS	=\
	allreduce.c

EXOBJS	=\
	allreduce.o


ALLOBJS	=	$(EXOBJS)
ALLBIN	=	$(Bin)/allreduce
ALLTGT	=	$(Bin)/allreduce

#@# Targets follow ---------------------------------

##all:	allreduce

objs:	$(ALLOBJS)

cleanobjs:
	rm -f $(ALLOBJS)

cleanbin:
	rm -f $(ALLBIN)

clean:	cleanobjs cleanbin

cleanall:	cleanobjs cleanbin






#all:allreduce
	
#clean:
#	-rm $(OBJ) $(Bin)/allreduce

#@# Dependency rules follow -----------------------------

all:$(EXOBJS)
	$(LD) $(LIBDIRS) $(LIBS) -o $(Bin)/allreduce $(EXOBJS) $(MYLIBS)

.c.o:
	$(CC) $(C_FLAGS) $(incDirs) $(LINKFLAGS) -o $@ -c $<


