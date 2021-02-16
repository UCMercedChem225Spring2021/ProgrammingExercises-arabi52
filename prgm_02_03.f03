      Program prgm_02_03
!
!     This program reads in slope of potential,  mass, box length, and quantum numbers
!     to create a Hamiltonian (T+V)  energy matrix for a particle in a box (1D).
!
      implicit none
      real   :: b, m, l, tmp1, tmp2
      real   :: PIB_1D_T_Element, PIB_1D_Modified_V_Element, PIB_1D_Modified_Hamiltonian_Element
      integer:: n1, n2
!
!
!     Start by asking the user for particle mass, length of 1D box,
!     the quantum number of first eigenstate (bra),and second eigenstate (ket)
!     in atomic units.
!
      write(*,*) 'What is the variable (b) for the potential energy operator?'
      read(*,*) b

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
!    Calculating potenital and kinetic  energy matrix elements
!
!
     tmp1=PIB_1D_Modified_V_Element(n1,n2,l,m,b)
     tmp2=PIB_1D_T_Element(m,l,n1,n2)

3000 format(1X,'Hamiltonian matrix element ',I5,',',I5,' is ',F12.5,'.')
     write(*,3000)  n1, n2, PIB_1D_Modified_Hamiltonian_Element(tmp2,tmp1)
!
!
      End Program prgm_02_03
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


      real FUNCTION PIB_1D_Modified_V_Element(n1,n2,l,m,b)
	implicit none
	real           :: b, m, l, tmp
        real, parameter:: PI = 4*ATAN(1.d0)
	integer        :: n1, n2
!
        if (n1.ne.n2) then
          tmp = b*l/PI**2
	  tmp = tmp*(1/(n1-n2)**2)*(COS(PI*(n1-n2))-1)-tmp*(1/(n1+n2)**2)*(COS(PI*(n1+n2))+1)
          PIB_1D_Modified_V_Element=tmp

          ! b*l/PI**2*(1/(n1-n2)**2*(COS(PI*(n1-n2))-1)) !-b*l/PI**2*(1/(n1+n2)**2*(COS(PI*(n1+n2))+1))
        else if (n1==n2) then
          PIB_1D_Modified_V_Element=0
        endIf

      end FUNCTION PIB_1D_Modified_V_Element

      real FUNCTION PIB_1D_Modified_Hamiltonian_Element(T,V)
         implicit none
         real::T,V
	 
         PIB_1D_Modified_Hamiltonian_Element=T+V
      end FUNCTION PIB_1D_Modified_Hamiltonian_Element
