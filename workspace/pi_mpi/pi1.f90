	program testpi1
!
! A serial piece of code calculating PI by a series expansion. 
! 
	REAL SUM , API
	INTEGER I , NN 
	NN = 1000
	sum = 0.0
	
	DO I = 1 , NN
	  SUM = SUM + 1.0/( 1.0 + (  (I-0.5)/REAL(NN)) **2 )
	ENDDO
	API = 4.0*SUM/NN
	WRITE(*,*) 'PI =' , API
	END
