!------------------------------------------------------------------------!
! Example of matrix diagonalization using LAPACK routine dsyev.f and the ! 
! Fortran 90 interface diasym.f90.                                       !
!------------------------------------------------------------------------!
! Input from file 'mat.dat' (matrix to be diagonalized):                 !
! line 1      : order of the symmetric matrix [M]                        !
! lines 2-n+1 : rows of the matrix                                       !
!------------------------------------------------------------------------!
! Output in file 'dia.dat':                                              !
! - eigenvalues                                                          !
! - eigenvectors (diagonalizing matrix [D])                              !
! - the original matrix [M] transformed by [D]; [1/D][M][D]              !
!------------------------------------------------------------------------!

!---------------!
 program diatest
!---------------!
 implicit none

 integer :: i,n
 real(8) :: m0(3,3),m1(3,3),m2(3,3),eig(3)

m0(1,:) = (/1, 2, 1 /)
m0(2,:) = (/2, 2, 3 /)
m0(3,:) = (/1, 3, 1 /)

 m1(:,:)=m0(:,:)

 call diasym(m1,eig,3)
   
 do i=1,3
    write(*,10)i,eig(i)
   10 format(I3,'   ',f14.8)
 enddo
 
 do i=1,3
    write(*,20)i,m1(:,i)
    20 format(i3,'   ',10f14.8)
 enddo

 end program diatest
!-------------------!

!---------------------------------------------------------!
!Calls the LAPACK diagonalization subroutine DSYEV        !
!input:  a(n,n) = real symmetric matrix to be diagonalized!
!            n  = size of a                               !
!output: a(n,n) = orthonormal eigenvectors of a           !
!        eig(n) = eigenvalues of a in ascending order     !
!---------------------------------------------------------!
 subroutine diasym(a,eig,n)
 implicit none

 integer n,l,inf
 real*8  a(n,n),eig(n),work(n*(3+n/2))

 l=n*(3+n/2)
 call dsyev('V','U',n,a,n,eig,work,l,inf)
 !Write(*,*) inf

 end subroutine diasym
!---------------------!

