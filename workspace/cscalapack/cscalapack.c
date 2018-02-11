/**********************************************************************
 *
 *   This is the first mpi example program that
 *  outputs 'Hello World'  from all the processors.
 * Note: A copy of this program will be running on 
 *        all the processors.
 *    
 *
 *********************************************************************/

#include <mpi.h>
#include <stdio.h>
#include "cscalapack.h"

int ictxt;


int main(int argc, char *argv[])
{
    int rank;		/* my rank in MPI_COMM_WORLD */
    int size;		/* size of MPI_COMM_WORLD */
    
/* Always initialise mpi by this call before using any mpi functions. */
    MPI_Init(&argc, &argv);
    
/* Find out how many processors are taking part in the computations.  */  
    MPI_Comm_size(MPI_COMM_WORLD, &size); 



/* Get the rank of the current process */      
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (rank == 0) {
       printf("Hello MPI world from C!\n");
    }

    printf("There are %d processes in my world, and I have rank %d\n",
           size, rank);

    MPI_Finalize();
    
    
    return 0;
} 

void sl_init( int *lictxt , int nprow, int npcol)
{
	
	/*     .. Scalar Arguments ..
	      INTEGER            lictxt, nprow, npcol
	*     ..
	*
	*  Purpose
	*  =======
	*
	*  SL_INIT initializes an NPROW x NPCOL process grid using a row-major
	*  ordering  of  the  processes. This routine retrieves a default system
	*  context  which  will  include all available processes. In addition it
	*  spawns the processes if needed.
	*
	*  Arguments
	*  =========
	*
	* lictxt   (global output) INTEGER
	*          ICTXT specifies the BLACS context handle identifying the
	*          created process grid.  The context itself is global.
	*
	*  NPROW   (global input) INTEGER
	*          NPROW specifies the number of process rows in the grid
	*          to be created.
	*
	*  NPCOL   (global input) INTEGER
	*          NPCOL specifies the number of process columns in the grid
	*          to be created.
	*
	*  =====================================================================
	*
	*     .. Local Scalars ..
	      INTEGER            IAM, NPROCS
	*     ..
	*     .. External Subroutines ..
	      EXTERNAL           BLACS_GET, BLACS_GRIDINIT, BLACS_PINFO, BLACS_SETUP	*/
    
    int rank,size;
    
    char sorder='r';
    int icontxt=-1;
    int iwhat=0;
    int myprow, mypcol;
    
    
    /*CALL BLACS_PINFO( IAM, NPROCS )*/
    blacs_pinfo_(&rank,&size); 
    
     if( size<1 )
     {
        if(rank==0) size=nprow*npcol;

      /*   CALL BLACS_SETUP( IAM, NPROCS )*/
       blacs_setup_(&rank, &size);
     }
     
     *
	/*     Define process grid
	*
	      CALL BLACS_GET( -1, 0, ICTXT )
	      CALL BLACS_GRIDINIT( ICTXT, 'Row-major', NPROW, NPCOL )
	*/
	 
	/*blacs_get_(&icontxt, &iwhat, &ictxt);*/
	blacs_gridinit_(lictxt, &sorder, &nprow, &npcol);  
 
    /*blacs_gridinfo__(&ic,&nr,&nc,&mr,&mc);*/
 
      
}                                    
