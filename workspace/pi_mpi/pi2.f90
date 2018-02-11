	program testpi2
	include 'mpif.h'
	REAL SUM , API
	INTEGER I , NN , NTOT ,IERR, NSIZE , IRANK ,I1,I2
	NN = 1000

	call MPI_INIT(ierr)

	call MPI_COMM_SIZE(MPI_COMM_WORLD,nsize , ierr)
	NTOT = NN*nsize
	call MPI_COMM_RANK(MPI_COMM_WORLD, irank , ierr) 
!	WRITE(*,*) ISIZE,IRANK
	I1 = 1+irank*NN
	I2 = (IRANK+1)*NN
	sum = 0.0 
	DO I = I1 , I2
	  SUM = SUM + 1.0/( 1.0 + ( (REAL(I)-0.5)/REAL(NTOT) ) **2 )
	ENDDO
	API = 4.0*SUM/NTOT
	WRITE(*,*) 'PARTIAL PI =' , API
	CALL MPI_FINALIZE(ierr) 
	END
