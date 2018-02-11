#include <stdio.h>
#define SQR(x) ( (x) * (x) )
 int main(int argc, char *argv[])
/*
   A serial piece of code calculating PI by a series expansion. */
 {                                                               
	float sum , api;
	int i , nn ; 
	nn = 1000 ;
	sum = 0.0 ;
	for (i=1;i<=nn;i++)
	{ 
	  sum = sum + 1.0/(   1.0 +  SQR( ((float)i-0.5)/ (float)nn   )  );
	}
	api = 4.0*sum/( (float) nn );
	printf ("pi =%f" , api );
 }
