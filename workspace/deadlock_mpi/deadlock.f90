! This badly thought out program demonstrates how we can  !
!  get into a deadlock situations during point to point   !
!  communications.                                        ! 

   PROGRAM TEST
   include 'mpif.h'
 
   integer rank;		! my rank in MPI_COMM_WORLD  
   integer size;		! size of MPI_COMM_WORLD  
   integer ierr
   
   CHARACTER*12 message , say 
   say ='hello'  
 
 ! Initialise MPI                      
    call MPI_INIT(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank ,ierr)
    
 ! print a message that process started  running   
    print * ,' Starting', rank        
     
! Now talk to a neighbour  
    call TALK_TO_NEIGH ( say , message  )
    call MPI_Finalize()
 

CONTAINS 
      SUBROUTINE TALK_TO_NEIGH ( tell ,  hear  )
!     This function receives a message from a neighbour and
!     and sends a message to the same neighbour.
      character*12 tell , hear
      integer talk_to ,   csize
      csize=12

!    if the rank is ODD talk to rank-1, if the rank is EVEN talk to rank+1
     if (mod(rank,2) .EQ. 1 ) then
   	talk_to = rank-1
     else
   	talk_to = rank + 1
     endif
 
     if ( talk_to .LT. size )then
       call MPI_Recv( hear , csize , MPI_CHARACTER , talk_to , 0,MPI_COMM_WORLD, status,ierr )
       print *, 'Processs',rank, 'heard ',hear,'from process',talk_to
       call MPI_Send( tell , csize , MPI_CHARACTER ,talk_to , 0 , MPI_COMM_WORLD, ierr)
      endif
      return
      END SUBROUTINE
   
   END

 
  
