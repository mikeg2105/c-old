	program diffuse4
! ----------------------------------------------------------------------
! EXAMPLE INTRODUCING : COLLECTIVE COMMUNICATIONS, REDUCTION OPERATION
!  This program tackles the same problem as in diffuse3.f90 except that:
! After the initial distribution we normalise all the initial field values
! so that the sum of them is = 1.0 
! We also enforce the rule that nothing escapes from the system
! by making the ends cycle back to the other side(i.e. what comes out of one end 
! goes into the other end. This is implemented by using the IF statements to
! redefine neighbours. We shall see in the next version that the process
! can be made easier vai a CARTESIAN TOPOLOGY. 
!
! ----------------------------------------------------------------------- 
        IMPLICIT NONE
	include 'mpif.h'

	INTEGER  :: irank , ileft , iright , nsize , irank , ierr , itstep , jj , I
	INTEGER  :: inunit
	INTEGER  :: itstep , NTSTEPS = 20 
	integer  :: status(MPI_STATUS_SIZE)
	REAL     :: NORMPDF , DPARAMS(3) , difpar , std , yright ,yleft , ynew , x , ytot ,ysum
	REAL     :: ainterval , DENSARRAY ( 1000 ) 
	CHARACTER*12 :: FNAME='density' 

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
!  open the output file .... 
	    INUNIT=10 	  
	    open(unit=INUNIT, FILE=FNAME, status='UNKNOWN' ) 	        

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
	    
		   
! Distribute the mass amongst the processors.
!
!             --
!           --  -- 
!         --      ---
!      ---           ---
!  ----                 ------
!  -----------------------------------------
! 1 , 2  , 3 , .....             n 

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
	call MPI_REDUCE ( ytot , ysum , 1 , MPI_REAL, MPI_SUM, 0 , MPI_COMM_WORLD,IERR) 
!debug	if(irank.eq.0 ) write(*,*) 'ysum=',ysum 
 	CALL MPI_BCAST( ysum , 1 , MPI_REAL , 0 , MPI_COMM_WORLD, IERR ) 
	ytot =ytot/ysum
!debug      write(*,*) 'rank ',irank , ytot
! 
! main iteration loop....
!
	 itstep = 0
	 
  10    CONTINUE
 
  	 call mpi_gather( YTOT , 1 , MPI_REAL , DENSARRAY,1,MPI_REAL,0,&
	& MPI_COMM_WORLD ,IERR ) 
 	   
	   if ( irank .EQ. 0 ) then
	     write(INUNIT, 150 ) ITSTEP, ( DENSARRAY(I) , I = 1,NSIZE )
150 FORMAT( ' TIME STEP:', I6 , 'DENSITY:', ( 5F7.3 ) ) 	   
	   endif   	

!	  
	call MPI_BARRIER( MPI_COMM_WORLD, IERR )
	    	            	 
! by setting neighbours of end cells to proc_null we are assuming 
! that particles migrate out as usual but never come back.
       
	  
	  ileft = irank -1 
	  if (ileft .lt. 0 ) ileft = nsize-1
	  iright = irank + 1
	  if (iright .gt. nsize-1 ) iright =  0
	  
! stop cyclic deadlock ....
	  IF ( mod(irank,2) .EQ. 1 ) THEN
	     call mpi_sendrecv( ytot ,1 ,MPI_REAL,iright,0,yright,1,MPI_REAL,iright,0,&
	    &                  MPI_COMM_WORLD,status, ierr)
	   
	     call mpi_sendrecv( ytot ,1 ,MPI_REAL,ileft,0,yleft,1,MPI_REAL,ileft,0,&
	    &                  MPI_COMM_WORLD,status, ierr) 
 
          ELSE
	     call mpi_sendrecv( ytot ,1 ,MPI_REAL,ileft,0,yleft,1,MPI_REAL,ileft,0,&
	    &                  MPI_COMM_WORLD,status, ierr)	  
	  
	     call mpi_sendrecv( ytot ,1 ,MPI_REAL,iright,0,yright,1,MPI_REAL,iright,0,&
	    &                  MPI_COMM_WORLD,status, ierr)
	   	  
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
