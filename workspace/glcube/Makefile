


include make_iceberg


OBJ = cube.o


cube: $(OBJ)
	gcc -o $(@) $(OBJ) $(LIB)

.c.o:
	gcc $(INCLUDE) -c $<

all: cube
	
clean:
	-rm $(OBJ) cube


