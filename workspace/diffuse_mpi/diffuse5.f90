	program diffuse5
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
! ----------------------------------------------------------------------- 
        IMPLICIT NONE
	include 'mpif.h'

	INTEGER  :: irank , ileft , iright , nsize , irank , ierr , itstep , jj , I
	INTEGER  :: inunit
	INTEGER  :: itstep , NTSTEPS = 20 
	integer  :: status(MPI_STATUS_SIZE)
	REAL     :: NORMPDF , DPARAMS(3) , difpar , std , yright ,yleft , ynew , x , ytot ,ysum
	REAL     :: ainterval , DENSARRAY(1000) 
	CHARACTER*12 :: FNAME='density'
	INTEGER   :: DIMS(3) , NDIMS  , NEWCOMM
	LOGICAL   ::  PERIODS(3) , REORDER

	real , parameter :: dom_leng = 1.0
 
	call MPI_INIT(ierr)

	call MPI_COMM_SIZE(MPI_COMM_WORLD,nsize , ierr)

	call MPI_COMM_RANK(MPI_COMM_WORLD, irank , ierr) 
	
	if ( nsize.le.1 ) then 
	  write(*,*) ' At least 2 processors are needed to run this! '
	  call MPI_FINALIZE(IERR)
	  STOP
	endif
! 
! Initialization and i/o for the master thread.	 
	
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
! diffusin per interval ..
            difpar = difpar*dom_leng/nsize
	    DPARAMS(1) = std
	    DPARAMS(2) = difpar	

	endif

 
! divide the domain amongst processes .	    
! we will use the mid-point of each interval in calculations.
! starting with ainterval/2 as the first intervals location.
	 ainterval = dom_leng/ nsize  		
	
! The model parameters has only been read into process 0 
! Broadcast these model parameters to all other processes...
!
	 CALL MPI_BCAST( DPARAMS , 2 , MPI_REAL , 0 , MPI_COMM_WORLD, IERR ) 
!  
   	  
	 std = DPARAMS(1)
	 difpar = DPARAMS(2)   
!
! so far we have been working within the default communicator group.
! we will now define a new communicator which sees the processes
! arranged in a 1D topology.
!
	NDIMS = 1
	DIMS(1) = NSIZE
	PERIODS(1) = .FALSE.
	REORDER = .FALSE.
	CALL MPI_CART_CREATE(MPI_COMM_WORLD, NDIMS , DIMS , PERIODS,REORDER, NEWCOMM,IERR )	    
		   
! Distribute the mass amongst the processors.
!
!             --
!           --  -- 
!         --      ---
!      ---           ---
!  ----                 ------
!  -----------------------------------------
! 1 , 2  , 3 , .....             n 

! get the rank again in ther new communicator domain. Incase or re-ordering !
	call MPI_COMM_RANK(NEWCOMM, irank , ierr) 
	
	IF( IRANK .EQ. 0 ) THEN
!  open the output file .... 
	    INUNIT=10 	  
	    open(unit=INUNIT, FILE=FNAME, status='UNKNOWN' ) 
	ENDIF	        
!?????? left here ....
! 
! by defining the initial distributions ..... 
!
          X = AINTERVAL*(IRANK + 0.5 )
	  YTOT = NORMPDF( X , DOM_LENG*0.5  , STD )
! 	 	   
!debug	  WRITE(*,*) IRANK ,'x=', X , YTOT 
!
! use the reduce function to sum the values of YTOT from all processors
! and copy it to the root(0)process.
	call MPI_REDUCE ( ytot , ysum , 1 , MPI_REAL, MPI_SUM, 0 , NEWCOMM,IERR) 
!debug	if(irank.eq.0 ) write(*,*) 'ysum=',ysum 
 	CALL MPI_BCAST( ysum , 1 , MPI_REAL , 0 , MPI_COMM_WORLD, IERR ) 
	ytot =ytot/ysum
!debug      write(*,*) 'rank ',irank , ytot
! 
! main iteration loop....
!
	 itstep = 0
	 
  10    CONTINUE
 
  	 call mpi_gather( YTOT , 1 , MPI_INTEGER , DENSARRAY,1,MPI_INTEGER,0,&
	&  		 NEWCOMM ,IERR ) 
 	   
	   if ( irank .EQ. 0 ) then
	     write(INUNIT, 150 ) ITSTEP, ( DENSARRAY(I) , I = 1,NSIZE )
150 FORMAT( ' TIME STEP:', I6 , 'DENSITY:', ( 5F7.3 ) ) 	   
	   endif   	

!	  
	  call MPI_BARRIER( MPI_COMM_WORLD, IERR )
       
	  CALL MPI_CART_SHIFT(NEWCOMM,0,1,ILEFT,IRIGHT,IERR )
 
 	  
! stop cyclic deadlock ....
	  IF ( mod(irank,2) .EQ. 1 ) THEN
	     call mpi_sendrecv( ytot ,1 ,MPI_REAL,iright,0,yright,1,MPI_REAL,iright,0,&
	    &                  NEWCOMM, status, ierr)
	   
	     call mpi_sendrecv( ytot ,1 ,MPI_REAL,ileft,0,yleft,1,MPI_REAL,ileft,0,&
	    &                  NEWCOMM,status, ierr) 
 
          ELSE
	     call mpi_sendrecv( ytot ,1 ,MPI_REAL,ileft,0,yleft,1,MPI_REAL,ileft,0,&
	    &                  NEWCOMM,status, ierr)	  
	  
	     call mpi_sendrecv( ytot ,1 ,MPI_REAL,iright,0,yright,1,MPI_REAL,iright,0,&
	    &                  NEWCOMM,status, ierr)
          ENDIF	   	  

	   ynew = ytot*(1.0-2.0*difpar) + difpar*(yleft+yright) 
	   ytot = ynew
  	    	  
   
	itstep = itstep + 1
        if(itstep.le.NTSTEPS )go to 10 
!!! end-of-main-loop block.....

	CLOSE (UNIT=INUNIT,STATUS='KEEP' )  
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
