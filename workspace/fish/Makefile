#Makefile for ising model

##### User configurable options #####
include make_tigger
### End User configurable options ###





ALLOBJS	=	$(EXOBJS)


#@# Targets follow ---------------------------------

all:	fisheries

objs:	$(ALLOBJS)

cleanobjs:
	rm -f $(ALLOBJS)

cleanbin:
	rm -f $(ALLBIN)

clean:	cleanobjs cleanbin

cleanall:	cleanobjs cleanbin

#@# Dependency rules follow -----------------------------
fisheries: $(EXOBJS)
	$(LD) -o $(Bin)/sum1 $(EXOBJS) $(MYLIBS) $(incDirs) $(libDirs) $(LD_FLAGS) $(LIBS)

$(EXOBJS):$(SRCS)
	$(CC) $(C_FLAGS) $(incDirs) $(LINKFLAGS) -o $@ -c $<


