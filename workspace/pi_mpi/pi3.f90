	program testpi3
!   A parallel code calculating segments of the series expansion of PI.
!   and then collecting the partial sums in the master thread to yield 
!   the total sum.                                                  	
	include 'mpif.h'
	REAL sum , totsum , api
	INTEGER i , NN , ntot ,ierr, nsize , irank ,i1,i2
	NN = 1000

	call MPI_INIT(ierr)

	call MPI_COMM_SIZE(MPI_COMM_WORLD,nsize , ierr)
	ntot = nn*nsize
	call MPI_COMM_RANK(MPI_COMM_WORLD, irank , ierr) 
!	WRITE(*,*) isize,irank
	I1 = 1+irank*NN
	I2 = (irank+1)*NN
	sum = 0.0 
	DO i = i1 , i2
	  sum = sum + 1.0/( 1.0 + ( (REAL(I)-0.5)/REAL(ntot) ) **2 )
	ENDDO
	call MPI_REDUCE( sum,totsum , 1 , MPI_REAL, MPI_SUM , 0 , &
       &                                         MPI_COMM_WORLD,ierr)
	api = 4.0*sum/ntot
	WRITE(*,*) 'PARTIAL PI =' , api
	if ( irank .eq.0 ) then 
	    bpi = 4.0*totsum/ntot 
	    write(*,*) 'Calculated PI:' , bpi 
	endif    
	CALL MPI_FINALIZE(ierr) 
	END
