
##### User configurable options #####
include make_iceberg
### End User configurable options ### 


OBJ = glvizsimple.o


glvizsimple: $(OBJ)
	$(CLINKER) -o $(@) $(OBJ) $(LIB)

.c.o:
	$(CC) $(INCLUDE) -c $<

all: glvizsimple
	
clean:
	-rm $(OBJ) glvizsimple


