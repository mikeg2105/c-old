#Makefile for ising model

##### User configurable options #####
include make_iceberg
### End User configurable options ###

OBJ = ising.o \
      ising_system.o\
      ising_particle.o\
      ising_statistics.o

#Compile
ising: $(OBJ)
	$(CC) -o $(@) $(OBJ) $(LIBS) $(OPTFLAGS) $(LINKFLAGS)
	
.cpp.o:
	$(CC) $(LINKFLAGS) -c $<

all:ising
	
clean:
	-rm $(OBJ) ising


