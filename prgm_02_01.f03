      Program prgm_02_01
!
!     This program reads in the mass, box length, and quantum numbers
!     to create a kinetic energy matrix for a particle in a box (1D).
!
      implicit none
      real::m,l,E
      real :: PIB_1D_T_Element
      integer:: n1, n2
!
!
!     Start by asking the user for particle mass, length of 1D box,
!     the quantum number of first eigenstate (bra),and second eigenstate (ket)
!     in atomic units.
!
      write(*,*) 'What is the mass of the partcle (in atomic units)?'
      read(*,*) m

      write(*,*) 'What is the length of the 1D box (in atomic units)?'
      read(*,*) l

      write(*,*) 'What is the quantum number of the first <bra| eigenstate?'
      read(*,*) n1

      write(*,*) 'What is the quantum number of the second |ket> eigenstate?'
      read(*,*) n2
!
!     
!    Calculating kinetic energy matrix elements
!
!

1000 format(1X,'Kinetic energy matrix element ', I5,','I5,' is ',F12.5,'.')
     write(*,1000)  n1, n2,  PIB_1D_T_Element(m,l,n1,n2)
!
!
!
!
      End Program prgm_02_01
!
      real FUNCTION PIB_1D_T_Element(m, l, n1, n2)
	implicit none
	real ::  m, l
        real, parameter:: PI = 4*ATAN(1.d0)
	integer::n1, n2
!
        if (n1==n2) then
          PIB_1D_T_Element=n1*n2*PI**2/2/m/l**2
        else if (n1.ne.n2) then
          PIB_1D_T_Element=0
        endIf

      end FUNCTION PIB_1D_T_Element
