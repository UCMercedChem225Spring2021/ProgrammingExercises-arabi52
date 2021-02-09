      Program prgm_01_03
!
!     This program reads TWO 3x3 matrices from user-provided input files. After the
!     files are opened and read, they are closed and then printed. The product of the
!     matrices is calculated, and this product is printed.
!
!
      implicit none
      integer,parameter::inFileUnitA=10,inFileUnitB=10
      integer::errorFlag,i,j
      real,dimension(3,3)::matrixInA, matrixInB, product
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
!     Call the subroutine PrintMatrix to print matrixInA.
!
      call PrintMatrix3x3(matrixInA)
      call PrintMatrix3x3(matrixInB)


!     *********  HERE I ADD MY PRODUCT SECTION  ************

     do i=1,3
	do j=1,3
	  product(i,j)=matrixInA(i,1)*matrixInB(1,j)+matrixInA(i,2)*matrixInB(2,j)+matrixInA(i,3)*matrixInB(3,j)
	endDo
     endDo

     call PrintMatrix3x3(product)

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
