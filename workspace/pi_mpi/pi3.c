#include <stdio.h>
#include <mpi.h>
#define SQR(x) ( (x) * (x) )
 int main(int argc, char *argv[])
/*
    A parallel code calculating segments of the series expansion of PI.
    and then collecting the partial sums in the master thread to yield 
    the total sum.                                                    */
 {   
        int nsize , irank  , ntot , i1 ,i2 , i , nn ;                                                            
	float sum , api , totsum;
        MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD , &nsize  ) ;
	MPI_Comm_rank(MPI_COMM_WORLD,  &irank ) ;
	nn = 100;
	ntot = nn*nsize;
	sum = 0.0 ;
	i1 = 1 + irank*nn;
	i2 = (irank+1)*nn;
	for (i=i1;i<=i2;i++)
	{ 
	  sum = sum + 1.0/(   1.0 +  SQR( ((float)i-0.5)/ (float)ntot   )  );
	}
	MPI_Reduce( &sum,&totsum , 1 , MPI_FLOAT, MPI_SUM , 0 , MPI_COMM_WORLD);
	api = 4.0*sum/( (float) ntot );
	printf ("partial pi =%f\n" , api );
	if ( irank == 0 ) { printf( " Calculated pi= %f\n" ,  4.0*totsum/( (float)ntot) ) ; 
	   }
	MPI_Finalize();
 }
