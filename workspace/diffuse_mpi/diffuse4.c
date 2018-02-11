/*	 								     *
! ----------------------------------------------------------------------
! EXAMPLE INTRODUCING : COLLECTIVE COMMUNICATIONS, REDUCTION OPERATION
!  This program tackles the same problem as in diffuse3.c (f90) except that;
! after the initial distribution we normalise all the initial field values
! so that the sum of them is = 1.0 
! We also enforce the rule that nothing escapes from the system
! by making the ends cycle back to the other side(i.e. what comes out of one end 
! goes into the other end. This is implemented by using the IF statements to
! redefine neighbours. We shall see in the next version that the process
! can be made easier vai a CARTESIAN TOPOLOGY. 
!
! -----------------------------------------------------------------------     */

 
  
#include <stdio.h>
#include <mpi.h>
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
/*	float periods[ndims] , dims[ndims];   */
	FILE *fpoint ;
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
	      scanf("%f" ,  &std );
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
	 
	   
/* for good book-keeping: synchronise everything after reading data. */

	 MPI_Barrier( MPI_COMM_WORLD );
/*	  printf("\n std and difpar for rank %d = %f  %f",irank,std,difpar);   */

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
	  
	  MPI_Reduce ( &ytot , &ysum , 1 , MPI_FLOAT, MPI_SUM, 0 , MPI_COMM_WORLD);  
	  
 	  MPI_Bcast( &ysum , 1 , MPI_FLOAT , 0 , MPI_COMM_WORLD ); 
	  ytot =ytot/ysum ;	  
	  printf("\nFor rank %d the initial distribution at x= %f is %f",irank, x , ytot ); 
	  
	 MPI_Barrier( MPI_COMM_WORLD );
	 
/* a time-steps.                  */ 

      for ( itstep = 1 ; itstep <= 10 ; itstep++ ) 
      {
      
 /* gather density information from all processes to process 0   */
    	   MPI_Gather( &ytot , 1 , MPI_FLOAT , &densarray,1,MPI_FLOAT,0, MPI_COMM_WORLD ); 
 	   
	   if ( irank == 0 ) {
	     printf("\nTime-Step:%d  ",itstep );
	     for ( ii=0; ii<=nsize-1; ii++ ) { printf(" %f ,",densarray[ii] ); } 
	     fprintf(fpoint,"\nTime-Step:%d  ",itstep );
	     for ( ii=0; ii<=nsize-1; ii++ ) { fprintf(fpoint," %f ,",densarray[ii] ); }  
 	   }

	    
      
 /* by setting neighbours of end cells to null_proc we are assuming 
    that particles migrate out as usual but never come back..            */
 
          ileft = irank -1 ;
	  if (ileft < 0 ) ileft = nsize-1 ;
	  iright = irank + 1 ;
	  if (iright > (nsize-1) ) iright = 0 ;
	  
/* because the send and receives are cyclic.There is a deadlock situation as none
 * can receive before sending first. So odd ones send.receive from left first 
 *  where as the even ones send receive from right first                         */  
	  if (irank%2 == 0 ) { 
	     MPI_Sendrecv( &ytot ,1 ,MPI_FLOAT,iright,0,&yright,1,MPI_FLOAT,iright,0,
	                 MPI_COMM_WORLD,&status1 );
	   
	     MPI_Sendrecv( &ytot ,1 ,MPI_FLOAT,ileft,0,&yleft,1,MPI_FLOAT,ileft,0,
	                    MPI_COMM_WORLD, &status2) ; 
          } else { 
	         MPI_Sendrecv( &ytot ,1 ,MPI_FLOAT,ileft,0,&yleft,1,MPI_FLOAT,ileft,0,
	                    MPI_COMM_WORLD, &status2) ; 
	        MPI_Sendrecv( &ytot ,1 ,MPI_FLOAT,iright,0,&yright,1,MPI_FLOAT,iright,0,
	                 MPI_COMM_WORLD,&status1 );
	   
	  }   

	  ynew = ytot*(1.0-2.0*difpar) + difpar*(yleft+yright) ;
	  ytot = ynew ;
  

/*
 * the call below stops treads running away from each other 
 * commment it out and observe the effect ...                 */
 
	   MPI_Barrier( MPI_COMM_WORLD);	 
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
   
