      Program prgm_01_03
!
!     This program reads two 3x3 matrices from user-provided input files.
!     After the files are opened and read, they are closed and the matrices
!     printed. Finally, the matrix product of these two matrices is formed
!     using the f90 intrinsic function MatMul. This final matrix is also
!     printed.
!
      implicit none
      integer,parameter::inFileUnitA=10,inFileUnitB=10
      integer::errorFlag,i
      real,dimension(3,3)::matrixInA, matrixInB, matrixProduct
      character(len=128)::fileNameA,fileNameB
!
!
!     Start by asking the user for the name of the data file.
!
      write(*,*)' What is the name of the FIRST  input data file?'
      read(*,*) fileNameA
      write(*,*)' What is the name of the SECOND input data file?'
      read(*,*) fileNameB
!
!     Opens files and read both matrices from files.
!
      open(unit=inFileUnitA,file=TRIM(fileNameA),status='old',  &
        iostat=errorFlag)
      if(errorFlag.ne.0) then
        write(*,*)' There was a problem opening the FIRST input file.'
        goto 999
      endIf

      do i = 1,3
        read(inFileUnitA,*) matrixInA(1,i),matrixInA(2,i),matrixInA(3,i)
      endDo
      close(inFileUnitA)

      open(unit=inFileUnitB,file=TRIM(fileNameB),status='old',  &
        iostat=errorFlag)
      if(errorFlag.ne.0) then
        write(*,*)' There was a problem opening the SECOND input file.'
        goto 999
      endIf

      do i = 1,3
        read(inFileUnitB,*) matrixInB(1,i),matrixInB(2,i),matrixInB(3,i)
      endDo
      close(inFileUnitB)
!
!     Call the subroutine PrintMatrix to print matrixInA and matrixInB.
!
      call PrintMatrix3x3(matrixInA)
      call PrintMatrix3x3(matrixInB)
!
!     Form matrixProduct using the intrinsic function MatMul. Then, print the
!     result.
!
      matrixProduct = MatMul(matrixInA,matrixInB)
      call PrintMatrix3x3(matrixProduct)

  999 continue
      End Program prgm_01_03


      Subroutine PrintMatrix3x3(matrix)
!
!     This subroutine prints a 3x3 real matrix. The output is written to StdOut.
!
      implicit none
      real,dimension(3,3),intent(in)::matrix
      integer::i
!
!     Format statements.
!
 1000 format(3(2x,f5.1))
!
!     Do the printing job.
!
	write(*,*)' Printing Matrix'
!
      do i=1,3
	write(*,1000) matrix(i,1),matrix(i,2),matrix(i,3)
      endDo
!
!
      return
      End Subroutine PrintMatrix3x3
