/*	 								     *
! ----------------------------------------------------------------------
! EXAMPLE INTRODUCING : CARTESIAN TOPOLOGIES
!  This program tackles the same problem as in diffuse4.f90 except that:
! We shall define a new communicator group in a cartesian topology
! configuration which is a much natural configuration for this problem.
! This will save us the details of bothering to calculate the ranks of
! neighbours etc and also make it easier to define cyclic or otherwise
! configurations.
!  
! Variations of the problem: Try periodic and non-periodic by setting 
!     the PERIODS TRUE or FALSE respectively to solve CLOSED/OPEN systems.
!
! -----------------------------------------------------------------------   */

 
  
#include <stdio.h>
#include <mpi.h>
#include <stdlib.h>
#include <math.h>	
	
const int ndims=1 ;
   	
  int main(int argc, char *argv[])	
 {
        float normpdf( float x ,float mu , float sigma);
	int buffer[10000] ;
	int irank , ileft , iright , nsize , ierr , i , ai , ii;
	int  itstep=0 ;
	MPI_Status status1 , status2 ;
	float dparams[3] , difpar , std , yright ,yleft ;
	float ynew , x ,ytot , ainterval , ysum;	 
	int periods[ndims] , dims[ndims] , reorder ;   
	FILE *fpoint ;
	MPI_Comm newcomm ;
        float densarray[100];
	
	const float  dom_leng = 1.0;
 
	 MPI_Init( &argc , &argv );

	 MPI_Comm_size(MPI_COMM_WORLD, &nsize);

	 MPI_Comm_rank(MPI_COMM_WORLD, &irank ) ; 
	 MPI_Buffer_attach(&buffer,1000);   
	
	if ( nsize <= 1 ) { 
	  printf ( " At least 2 processors are needed to run this job! ");
	  MPI_Finalize();
	  return (-1) ;
	    }
/*
 * initialization and user parameters input.
 */
	 
/*    master thread will read the user input and pass it onto others */		
	if ( irank == 0 ) {
	    printf( " total number of processors:%d", nsize );
	    printf( "\n  total length of the domain is taken to be 1 units");
            printf(" \n i.e. length is normalised. the initial distribution is assumed to ");
            printf(" \n center in the middle of the domain (i.e. at 0.5 ) " );
   
            std=-0.1;
            while (std <= 0.0 ) 
	      {
	      printf("\n Enter standard deviation of the initial distribution:" );
	      /*scanf("%f" ,  &std );*/
	      if(argc>1)
	      	std=atof(argv[1]);
	      else
	      	std=0.1;
	      if( std <=0 ) printf("\nmust be positive"); 
	      } 

  
            printf("\n\nThe diffusion parameter will be specified as fraction of ");
	    printf("\nmass per unit length per time-step, must be between 0.0 and 1.0\n"); 
	    printf("\n Enter Diffusion Parameter:" ); 
	    scanf("%f" , &difpar ) ;

            difpar = difpar*dom_leng/nsize;		    
	    dparams[0] = std ;
	    dparams[1] = difpar	; 
/* open the output file  */
	    fpoint = fopen( "outputall","w" );
	    fprintf(fpoint, " Output from all processes...." ) ;	               
        }
  
/* for good book-keeping: synchronise everything after reading data.  */  

	 MPI_Barrier( MPI_COMM_WORLD );
         
	
/* The model parameters has only been read into process 0 
 ! Broadcast these model parameters to all other processes...
 !                                                            */
 	 MPI_Bcast( &dparams , 2 , MPI_FLOAT , 0 , MPI_COMM_WORLD ); 
   	  
	 std = dparams[0];
	 difpar = dparams[1]  ; 
	 

	
	dims[0] = nsize;
	periods[0] = 1;   /* periodicity  is given here */
	reorder =  0 ; 
	MPI_Cart_create(MPI_COMM_WORLD, 1 , dims , periods,reorder, &newcomm ) ;
		    
 /* get the rank again in ther new communicator domain. Incase or re-ordering  */
	MPI_Comm_rank(newcomm, &irank );
			   	   
 
/* divide the domain amongst processes .	    
 ! we will use the mid-point of each interval in calculations.
 ! starting with ainterval/2 as the first intervals location. */
 
	 ainterval = dom_leng/ nsize ; 
	 		
/*	    	
		   
! distribute the mass amongst the processors.
!
!             --
!           --  -- 
!         --      ---
!      ---           ---
!  ----                 ------
!  -----------------------------------------      
! 1 , 2  , 3 , .....             n                       /*



/* initialise distribution during the first time step.  */

          x = ainterval*(irank + 0.5 ) ;
	  ytot = normpdf( x , dom_leng*0.5  , std ) ;
	  
	  MPI_Reduce ( &ytot , &ysum , 1 , MPI_FLOAT, MPI_SUM, 0 , newcomm);  
	  
 	  MPI_Bcast( &ysum , 1 , MPI_FLOAT , 0 , MPI_COMM_WORLD ); 
	  ytot =ytot/ysum ;	  
	  printf("\nFor rank %d the initial distribution at x= %f is %f",irank, x , ytot ); 
	  
	 MPI_Barrier( newcomm);
	 
/* a time-steps.                  */ 

      for ( itstep = 1 ; itstep <= 10 ; itstep++ ) 
      {
      
 /* gather density information from all processes to process 0   */
    	   MPI_Gather( &ytot , 1 , MPI_FLOAT , &densarray,1,MPI_FLOAT,0, newcomm ); 
 	   
	   if ( irank == 0 ) {
	     printf("\nTime-Step:%d  ",itstep );
	     for ( ii=0; ii<=nsize-1; ii++ ) { printf(" %f ,",densarray[ii] ); } 
	     fprintf(fpoint,"\nTime-Step:%d  ",itstep );
	     for ( ii=0; ii<=nsize-1; ii++ ) { fprintf(fpoint," %f ,",densarray[ii] ); }  
 	   }

           MPI_Cart_shift(newcomm,0,1, &ileft, &iright) ;	    
	  
/* because the send and receives are cyclic.There is a deadlock situation as none
 * can receive before sending first. So odd ones send.receive from left first 
 *  where as the even ones send receive from right first                         */  
	  if (irank%2 == 0 ) { 
	     MPI_Sendrecv( &ytot ,1 ,MPI_FLOAT,iright,0,&yright,1,MPI_FLOAT,iright,0,
	                 newcomm,&status1 );
	   
	     MPI_Sendrecv( &ytot ,1 ,MPI_FLOAT,ileft,0,&yleft,1,MPI_FLOAT,ileft,0,
	                    newcomm, &status2) ; 
          } else { 
	         MPI_Sendrecv( &ytot ,1 ,MPI_FLOAT,ileft,0,&yleft,1,MPI_FLOAT,ileft,0,
	                    newcomm, &status2) ; 
	        MPI_Sendrecv( &ytot ,1 ,MPI_FLOAT,iright,0,&yright,1,MPI_FLOAT,iright,0,
	                 newcomm,&status1 );
	   
	  }   

	  ynew = ytot*(1.0-2.0*difpar) + difpar*(yleft+yright) ;
	  ytot = ynew ;
  

/*
 * the call below stops treads running away from each other 
 * commment it out and observe the effect ...                 */
 
	   MPI_Barrier( newcomm);	 
       }                               
      MPI_Finalize(); 
      if( irank == 0 )printf("\n" );
      return (0) ;
     }
  
     float normpdf( float x ,float mu , float sigma) 
	 {
	   double  aaa, pi , asigma ;
	   pi= 4.0*atan(  (double) 1.0) ;
	   asigma = (double) sigma;
	   if (asigma <= 0.0 ) asigma = 0.001;
	   aaa = -0.5* pow ( ((x-mu)/asigma) , (double)2.0 ) ;
	   aaa = exp(aaa )/( asigma*sqrt(2.0*pi) ) ;
	   return ( (float) aaa) ;
	} 	
   
