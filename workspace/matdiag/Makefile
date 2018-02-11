#Makefile for matdiag model

##### User configurable options #####
include make_felix
### End User configurable options ###



SRCS	=\
	matdiag.cpp \
	cmatdiag.cpp

EXOBJS	=\
	$(oDir)/matdiag.o \
	$(oDir)/cmatdiag.o

ALLOBJS	=	$(EXOBJS)
ALLBIN	=	$(Bin)/matdiag
ALLTGT	=	$(Bin)/matdiag

#@# Targets follow ---------------------------------

##all:	matdiag

objs:	$(ALLOBJS)

cleanobjs:
	rm -f $(ALLOBJS)

cleanbin:
	rm -f $(ALLBIN)

clean:	cleanobjs cleanbin

cleanall:	cleanobjs cleanbin






#all:matdiag
	
#clean:
#	-rm $(OBJ) $(Bin)/matdiag

#@# Dependency rules follow -----------------------------

all:$(EXOBJS)
	$(LD) -g $(LIBS) -o $(Bin)/matdiag $(EXOBJS) $(MYLIBS)

.cpp.o:
	$(CC) $(C_FLAGS) $(incDirs) $(LINKFLAGS) -o $@ -c $<


