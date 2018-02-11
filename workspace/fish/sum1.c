      PROGRAM sum1
C
C     Program to sum all elements of a two dimensional array.
C
C     Version 1: Fortran 77
C                This version fails because subroutine sumsub
C                uses the wrong elements of array ra in the main
C                program.
C
      PARAMETER (n=10)
      REAL ra(n,n)
      INTEGER i, j, n1, nsum

      DO 20 i=1,n
        DO 10 j=1,n
          ra(j,i) =1000 
   10   CONTINUE
   20 CONTINUE

      WRITE (*,'("Input matrix size:")')
      READ (*,*) n1

      DO 40 i=1,n1
        DO 30 j=1,n1
          ra(j,i) = (i-1)*n1 + j
   30   CONTINUE
   40 CONTINUE

      CALL sumsub(ra,n1,nsum)
      WRITE (*,'("Sum for matrix of size ",I3," = ",I10)') n1,nsum

      END

      SUBROUTINE sumsub(ra,n1,nsum)

      REAL ra(n1,n1)
      INTEGER n1, nsum, i, j

      nsum = 0
      DO 20 i=1,n1
        DO 10 j=1,n1
          nsum = nsum + ra(j,i)
   10   CONTINUE
   20 CONTINUE

      RETURN
      END
