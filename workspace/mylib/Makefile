#simple Makefile for mylib

#Option -Wno-deprecated removes compiler warnings
#about use of deprecated libaries e.g. iostream.h fstream.h
# which are not part of the iso standard should
#use the standard libraries <iostream> and <fstream>

OBJ = vec.o \
	  vecpair.o \
	  mat.o


#Compile
all : $(OBJ)
	ar -rv mylib.a $(OBJ)

	
vec.o:
	g++   -Wno-deprecated -c vec.cc

vecpair.o:
	g++   -Wno-deprecated -c vecpair.cc
	
mat.o:
	g++   -Wno-deprecated -c mat.cc	
	
clean :
	rm mylib.a $(OBJ)


