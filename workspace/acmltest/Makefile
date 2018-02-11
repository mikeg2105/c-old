#Makefile for acml test

##### User configurable options #####
include make_iceberg
### End User configurable options ###




OBJ = acml.o 

#Compile
acml: $(OBJ)
	$(CC) -o $(@) $(OBJ) $(INCLUDE) $(LIBS) $(FLAGS) $(LINKFLAGS)
	
.cpp.o:
	$(CC) $(INCLUDE) $(LINKFLAGS) $(LIBS) -c $<

all:acml
	
clean:
	-rm $(OBJ) acml


