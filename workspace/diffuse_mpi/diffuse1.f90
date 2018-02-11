	PROGRAM diffuse
!
! EXAMPLE INTRODUCING : POINT TO POINT COMMUNICATIONS....
!
! This program will simulate the mechanics of diffusion as follows:
! Given that the number of processors  we can utilise is <n> 
!                               divide the domain into <n> regions. 
!  Impose an initial distribution of particles onto these <n> regions 
!                      ( for example a normal probability distribution).
! Simulate diffusion by allowing <f> percent of particles from each domain
! during a time-step to escape to the neighbouring domains on either side 
!                     of it. 1-D only so 2 neighbours. 
! NOTES: Particles that escape through each end never return.
! Simulate for a given number of time steps (m) 
! List or plot the intermediate and final distributions.
! 
        IMPLICIT NONE
	include 'mpif.h'
	INTEGER, PARAMETER :: NDIMS=1
	INTEGER  :: irank , ileft , iright , nsize , irank , ierr , itstep , jj
	INTEGER  :: ITSTEP = 0 
	integer  :: status(MPI_STATUS_SIZE)
	REAL     :: NORMPDF , DPARAMS(3) , difpar , std , yright ,yleft , ynew , x , ytot
	REAL     :: ainterval
	integer  :: periods(ndims) , dims(ndims)
	real , parameter :: dom_leng = 1.0
 
	call MPI_INIT(ierr)

	call MPI_COMM_SIZE(MPI_COMM_WORLD,nsize , ierr)

	call MPI_COMM_RANK(MPI_COMM_WORLD, irank , ierr) 
	
	if ( nsize.le.1 ) then 
	  write(*,*) ' At least 2 processors are needed to run this job! '
	  call MPI_FINALIZE(IERR)
	  STOP
	endif
! 
! Initialization and User Parameters Input.
!
	 
!  Master thread will read the user input and pass it onto others.		
	if ( irank.eq. 0 ) then
	    write(*,*)'total number of processors:', nsize
	    write(*,120)
 120 format ( 'Total length of the domain is taken to be 1 units '/ &
    &' i.e. Lenght is normalised. The initial distribution is assumed to '/ &
    & 'center in the middle of the domain (i.e. at 0.5 )' )
 121  	    write(*,122) 
 122 format (  'Enter standard deviation of the initial distribution:' )
	    read(*,*, ERR=121 ) std
	    if ( std.le.0.0 ) go to 121

	    write(*,124) 
 124 format( 'The diffusion parameter will be specified as fraction of mass'/&
     &' per unit length per time-step must be between 0.0 and 1.0 ' )
            write (*,126) 
 126 format ( 'Enter diffusion parameter :' )
 	    read(*,* ) difpar
! diffusion per interval ..
            difpar = difpar*dom_leng/nsize		    
	    DPARAMS(1) = std
	    DPARAMS(2) = difpar	    

	endif

! For good book-keeping: synchronise everything after reading data.
	call MPI_BARRIER( MPI_COMM_WORLD, IERR )
	
! The input parameters ( DPARAMS array) has only been read into thread 0 .  
! Now pass them onto all the other threads via SEND and RECEIVEs.
 	
	IF ( IRANK .EQ. 0 ) THEN 
	  DO JJ = 1 , NSIZE-1
	 CALL MPI_SSEND( DPARAMS , 2 , MPI_REAL, JJ , 0 , MPI_COMM_WORLD,IERR)	
	  END DO	    
        ELSE 
	  CALL MPI_RECV( DPARAMS , 2 , MPI_REAL , 0 , 0 ,MPI_COMM_WORLD,STATUS,IERR)
	   std = DPARAMS(1)
	   difpar = DPARAMS(2)
	ENDIF
   
	   
! For good book-keeping: synchronise everything after reading data.
	call MPI_BARRIER( MPI_COMM_WORLD, IERR )
	
! divide the domain amongst processes .	    
! we will use the mid-point of each interval in calculations.
! starting with ainterval/2 as the first intervals location.
	 ainterval = dom_leng/ nsize  
	 		
	    	
		   
! Distribute the mass amongst the processors.
!
!             --
!           --  -- 
!         --      ---
!      ---           ---
!  ----                 ------
!  -----------------------------------------
! 1 , 2  , 3 , .....             n 

     


	itstep = 0
	
! initialise distribution during the first time step.

        X = AINTERVAL*(IRANK + 0.5 )
	YTOT = NORMPDF( X , DOM_LENG*0.5  , STD ) 
	WRITE(*,*) IRANK ,'The Initial distribution at x=', X ,' is', YTOT 
	  
  10    continue
	 	 

	 
! a time-step. 

! by setting neighbours of end cells to null_proc we are assuming 
! that particles migrate out as usual but never come back..
 
	  ileft = irank -1 
	  if (ileft .lt. 0 ) ileft = mpi_proc_null
	  iright = irank + 1
	  if (iright .gt. nsize-1 ) iright = mpi_proc_null
	  call mpi_sendrecv( ytot ,1 ,MPI_REAL,iright,0,yright,1,MPI_REAL,iright,0,&
	 &                   MPI_COMM_WORLD,status, ierr)
	   
	  call mpi_sendrecv( ytot ,1 ,MPI_REAL,ileft,0,yleft,1,MPI_REAL,ileft,0,&
	 &                   MPI_COMM_WORLD,status, ierr) 
!! debug	   if (iright.eq.mpi_proc_null ) write(*,*) irank, yright
	  ynew = ytot*(1.0-2.0*difpar) + difpar*(yleft+yright) 
	  ytot = ynew
	  write(*,*) ' Time Step:',itstep,' Rank:' ,irank, ' Distribution:',ytot
! call to flush writes out the pending outputs, clearing the output buffer. 
! without this call we may get output from different time steps intermingled.
	  call flush(2) 
	  	
	  
	 
!
! The call below stops treads running away from each other 
! Commment it out and observe the effect ...
	call MPI_BARRIER( MPI_COMM_WORLD, IERR )
	    	  
	  
! increment the time step counter and stop if step 10 is reached. 
! 	  
	itstep = itstep + 1
        if(itstep.le.10 )go to 10  
	
	CALL MPI_FINALIZE(IERR) 
	 
	END
	
	REAL FUNCTION NORMPDF ( X, MU , SIGMA )
! Given a normal distribution with mean=MU and standard deviation=SIGMA
! it returns the value of the distribution function at location X. 
	REAL , INTENT(IN) :: X , MU ,SIGMA 
	PI = 4.0* ATAN (1.0 ) 
! botch to avoid negative standard deviation.
	ASIGMA = SIGMA
	IF (ASIGMA .LE. 0 ) ASIGMA = 0.001
 
	NORMPDF =  EXP( -0.5*(( X -MU )/ASIGMA )**2  )/( ASIGMA*SQRT(2.0*PI ) )

	RETURN
	END
