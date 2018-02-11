#Makefile for diffuse model

##### User configurable options #####
include make_iceberg
### End User configurable options ###

OBJ = diffuse5.o

#Compile
diffuse_mpi: $(OBJ)
	$(CC) -o $(@) $(OBJ) $(LIBS) $(OPTFLAGS) $(LINKFLAGS)
	
.cpp.o:
	$(CC) $(LINKFLAGS) -c $<

all:diffuse_mpi
	
clean:
	-rm $(OBJ) diffuse_mpi


