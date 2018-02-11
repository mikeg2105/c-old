#include <mpi.h>
#include <stdio.h>
 int rank;		/* my rank in MPI_COMM_WORLD */
 int size;		/* size of MPI_COMM_WORLD */

int main(int argc, char *argv[])
{ 

    char message[12] ;
    char say[12]= {'h','e','l','l','o','\0'};
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);    
    printf(" starting %d \n" , rank );
    talk_to_neigh ( say , message , 12 );
    MPI_Finalize();
}
/* extern int rank, size; */
int talk_to_neigh ( char *tell , char *hear , int n )
{ int talk_to;
MPI_Status status;
talk_to = (rank % 2) ? rank-1 : rank +1 ;
if ( talk_to < size ){
 int nn;
  MPI_Sendrecv(tell,n, MPI_CHAR , talk_to, 0 ,hear , nn , MPI_CHAR , 
      talk_to , 0,MPI_COMM_WORLD, &status );
      printf(" Processs %d heard %s from process %d \n", rank,hear,talk_to );
}
return;
}

 
  
