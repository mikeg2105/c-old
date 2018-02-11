
##### User configurable options #####
include make_felix
### End User configurable options ### 


OBJ = sovizsimple3.o


sovizsimple: $(OBJ)
	$(CLINKER) -o $(@) $(OBJ) $(LDFLAGS) $(LIBS)

.c.o:
	$(CC) $(INCLUDE) -c $<

all: sovizsimple
	
clean:
	-rm $(OBJ) sovizsimple


