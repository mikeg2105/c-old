/* This badly thought out program demonstrates how we can  *
*  get into a deadlock situations during point to point    *
*  communications.                                         */ 
#include <mpi.h>
#include <stdio.h>
#define BUFFSIZ 1024
  int bsize;
  char *buff;
 int rank;		/* my rank in MPI_COMM_WORLD */
 int size;		/* size of MPI_COMM_WORLD */

int main(int argc, char *argv[])
{ 

    char message[12] ;
    char say[12]= {'h','e','l','l','o','\0'};
/* Initialise MPI                                */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    
    buff = (char*) malloc(BUFFSIZ) ;
    MPI_Buffer_attach( buff,BUFFSIZ); 
    
/* print a message that process started  running  */        
    printf(" starting %d \n" , rank );
    
/* Now talk to a neighbour */
    talk_to_neigh ( say , message , 12 );
    MPI_Finalize();
}


int talk_to_neigh ( char *tell , char *hear , int n )
/* This function receives a message from a neighbour and  *
/* and sends a message to the same neighbour.             */
{ int talk_to;
  MPI_Status status;
  MPI_Request request1 , request2;

 /* if the rank is ODD expression is 1 i.e. TRUE so talk to rank-1    *
  * If the rank is EVEN expression is 0 i.e.FALSE so talk to rank+1   */

talk_to = (rank % 2) ? rank-1 : rank +1 ;
if ( talk_to < size ){
  MPI_Irecv( hear , n , MPI_CHAR , talk_to , 0,MPI_COMM_WORLD,  &request1);
  printf(" Processs %d heard %s from process %d \n", rank,hear,talk_to );
  MPI_Ibsend( tell , n , MPI_CHAR ,talk_to , 0 , MPI_COMM_WORLD, &request2);
}
return;
}

 
  
