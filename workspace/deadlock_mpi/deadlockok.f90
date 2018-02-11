! This is the modified version of deadlock.f90 which avoids  !
! deadlock situation by using sen-receive calls.             !

   PROGRAM TEST
   use mpi
 
   integer rank;		! my rank in MPI_COMM_WORLD  
   integer size;		! size of MPI_COMM_WORLD  
   integer ierr
   
   CHARACTER*16  :: say='HELLO FROM'  , message
   
 
 ! Initialise MPI                      
    call MPI_INIT(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank ,ierr)
    
  
! Now talk to a neighbour  
    call TALK_TO_NEIGH ( say , message  )
    call MPI_BARRIER( MPI_COMM_WORLD, IERR )   
    call MPI_Finalize(ierr)
 

CONTAINS 
      SUBROUTINE TALK_TO_NEIGH ( tell ,  hear  )
!     This function receives a message from a neighbour and
!     and sends a message to the same neighbour.
      character*16 tell , hear
      integer talk_to , status(MPI_STATUS_SIZE) , ierr 
! add the processor rank to the message string.   
      WRITE( TELL(12:),'(I3)' ) rank
      
!    if the rank is ODD talk to rank-1, if the rank is EVEN talk to rank+1
     if (mod(rank,2) .EQ. 1 ) then
   	talk_to = rank-1
     else
   	talk_to = rank + 1
     endif
  
     if ( talk_to .LT. size )then
       call MPI_Sendrecv(tell ,16, MPI_CHARACTER , talk_to, 0 ,hear , 16 , &
      &                   MPI_CHARACTER, talk_to , 0,MPI_COMM_WORLD, status, ierr )
       write(*,*) 'Processor' , rank, 'received=', hear
      endif
      return
      END SUBROUTINE
   
   END

 
  
