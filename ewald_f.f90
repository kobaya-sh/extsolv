subroutine ewald_f ( charge, natom, f_ewald, numex, inb )
implicit none
include 'size.inc'
include 'const.inc'
include 'mmpara.inc'
include 'gau.inc'
include 'mmcounter.inc'

integer            :: natom
real*8,intent(in)  :: CHARGE(m_natom)
integer            :: NUMEX(m_natom)     ! numex(i) = number of excluded atoms of larger index than i with i
integer            :: INB(m_nnb)         ! inb(i) : list of excluded atoms

real*8,intent(out) :: f_ewald(m_natom,3) ! Force
real*8             :: f_r(m_natom,3)     ! R-space Coulomb force
real*8             :: f_k(m_natom,3)     ! K-space Coulomb force
real*8             :: f_ex(m_natom,3)    ! Excluded Pair force
real*8             :: f_scalar           ! Temporaly force
real*8             :: pos_i(3)           ! position of atom i
real*8             :: pos_j(3)           ! minimum image of crd(j,1:3)
real*8             :: len                ! r_ijn
real*8             :: len_eta            ! r_ijn/eta
real*8             :: r_ijn(3)           ! r_i-r_j ( minimum image )

real*8             :: vol                ! cell volume:vol=cell(1)*cell(2)*cell(3)
real*8             :: eta                ! parameter
real*8             :: per_eta_cube       ! eta^(-3)
real*8             :: two_per_sqrtpi     ! 2/sqrt(pi)
real*8             :: two_pi             ! 2*pi=6.2831853
real*8             :: pi2eta2            ! pi^2*eta^2
!real*8             :: basis_r(3,3)       ! basis set in real space
!real*8             :: basis_k(3,3)       ! basis set in reciprocal space
integer            :: i_k,j_k,k_k        ! counter to box
real*8             :: k_b(3)             ! k/B
real*8             :: k_normsq           ! | k/b |
real*8             :: k_br_i             ! dot_product(k_b,r_i) 
real*8             :: k_br_j             ! dot_product(k_b,r_j) 
real*8             :: max_k2             ! k cutoff if ( |k|^2 > max_k2 )
integer            :: i_max,j_max,k_max  ! k cutoff by element of k_vector
integer            :: i_numex            ! counter of numex
integer            :: p_numex            ! pointer
real*8,external    :: length,length_vec,erfc

intrinsic dot_product,dcos,dsin,dexp,dabs,dsqrt,int

!****************************************************************************!
!                                                                            !
!                          [                                                 !
!                          [                                                 !
!               q_i        [                 ---->                           !
!    (f_r)_i = ----- sigma [   sigma   q_j * r_ijn *                         !
!              eta^3   n   [ j/=i,n/=0                                       !
!                          [                                                 !
!                          [                                                 !
!                                                                            !
!                2        (  r_ijn   ) r_ijn        r_ijn      ]             !
!             -------- exp(-(-----)^2) ----- + erfc(-----)     ]             !
!             sqrt(pi)    (   eta    )  eta          eta       ]             !
!          ( ----------------------------------------------- ) ]             !
!                                 r_ijn                        ]             !
!                                (-----)^3                     ]             !
!                                  eta                         ]             !
!                                                                            !
!              2*q_i       [ exp[(-pi^2*k^2)/(eta^2*B^2)]                    !
!    (f_k)_i = ----- sigma [ ---------------------------- *                  !
!                V     k   [           (k/B)^2                               !
!                                                                            !
!                                    -->   --> -->     ]   -->               !
!             ( sigma q_j * sin(2*pi(k/B)*(r_i-r_j)) ) ] *  k                !
!                 j                                    ]                     !
!                                                                            !
!      -->   -->          -->     --> -->     -->     ---->                  !
!    ( k/B *  n  = 0, so (k/B) * (r_i-r_j) = (k/B) * (r_ijn) )               !
!                                                                            !
!                 -->                                                        !
!    (f_self)_i =  0  ( because e_self is constant )                         !
!                                                                            !
!                              --> -->                                       !
!                    [         r_i-r_j                                       !
!    (f_ex)_i= sigma [ q_I*q_j*-------*                                      !
!               j/=i [          r_ij^3                                       !
!                                                                            !
!               [    2*r_ij       { (r_ij)^2}    (r_ij) ] ]                  !
!               [ ------------*exp{-(----)  }-erf(----) ] ]                  !
!               [ sqrt(pi)*eta    { ( eta)  }    ( eta) ] ]                  !
!                                                                            !
!                                                                            !
!    i          : atom to consider force                                     !
!    j          : atom which interacts with atom i                           !
!    V          : Volume of unit cell                                        !
!    eta        : parameter ( arbitrary,but decide after paragraph )         !
!    n(vector)  : integer vector in real space                               !
!    k(vector)  : integer vector in reciprocal space                         !
!    B          : box size                                                   !
!    k/B        : reciprocal lattice vector                                  !
!    r_i(vector): atom position ( origin is arbitrary )                      !
!    q_i        : charge ( to put coefficient 1, divided by 18.3332 )        !
!                                                                            !
!    NOTICE.1:                                                               !
!      Sum for R-space converges rapidly, so sum for n is operated           !
!      only over n = (0,0,0) ( minimum image ).                              !
!                                                                            !
!    NOTICE.2:                                                               !
!      Sum of k is rigorously over N(set of integer)^3, but it is truncated. !
!      In Allen's program, k is chosen to satisfy |k|<=kmax.                 !
!                                                                            !
!    NOTICE.3:                                                               !
!      In present program, lattice vector is assumed to be orthogonal.       !
!      Therefore k/B = (kx/box_x, ky/box_y, kz/box_z). In general, namely    !
!      in case of                                                            !
!                 (ax,bx,cx)                                                 !
!         (a,b,c)=(ay,by,cy)=g,                                              !
!                 (az,bz,cz)                                                 !
!      k/B is dual vector of a, b, and c, which is obtained as COLUMN        !
!      vector of inverse of matrix g.                                        !
!                                                                            !
!    NOTICE.4:                                                               !
!      According to Komeiji's text, error of Ewald sum is not more than      !
!      exp(-p) when                                                          !
!        p=(cutoff/eta)^2=(pi*eta*max_k/B)                                   !
!      and cell is cubic. From this fact,                                    !
!        max_k = B*cutoff/(pi*eta**2) ( dimensionless number )               !
!        eta   = cutoff/sqrt(p)                                              !
!      is obtained. Cutoff in K-space is performed when |k| > max_k. In      !
!      this program, eta, max_k, and k_limit is chosen to satisfy above      !
!      formula.                                                              !
!                                                                            !
!****************************************************************************!

f_ewald=0.0d0
if ( .not. ( is_pbc .and. is_ewald ) ) return
!
!   1. Make cell
!
idbg = .false.
!idbg=.true.
!
basis_r=0.0d0
basis_k=0.0d0
do i_k=1,3
  basis_r(i_k,i_k)=cell(i_k)
  basis_k(i_k,i_k)=1.0d0/cell(i_k) ! basis_k=(basis_r^-1)^t
enddo
vol=dabs( basis_r(1,1)*basis_r(2,2)*basis_r(3,3) + &
          basis_r(2,1)*basis_r(3,2)*basis_r(1,3) + &
          basis_r(3,1)*basis_r(1,2)*basis_r(2,3) - &
          basis_r(1,1)*basis_r(3,2)*basis_r(2,3) - &
          basis_r(2,1)*basis_r(1,2)*basis_r(3,3) - &
          basis_r(3,1)*basis_r(2,2)*basis_r(1,3) )
if ( idbg ) then
  write(0,*) 'basis in R-space ( column, angstrom    )'
  do i_k=1,3
    write(0,*) (basis_r(i_k,j_k),j_k=1,3)
  enddo
  write(0,*) 'basis in K-space ( column, angstrom^-1 )'
  do i_k=1,3
    write(0,*) (basis_k(i_k,j_k),j_k=1,3)
  enddo
  write(0,*) 'cell volume in R-space ( angstrom^3 )'
  write(0,*) vol
endif
idbg = .false.
!
!   2. Make Parameter
!
!idbg = .true.
eta     = cutoff / dsqrt(p)
max_k2  = ( ( vol**0.33333333d0 * cutoff ) / ( pi * eta**2 ) ) ** 2
if ( idbg ) write(0,'(a,2f12.4,i6)') 'eta,max_k= ', eta, dsqrt(max_k2)
!
!   3. R-space Force
!
idbg = .true.
f_r=0.0d0
two_per_sqrtpi=2.0d0/dsqrt(pi)
do i=1,natom
  pos_i = crd(i,1:3)
  do j=i+1,natom
    pos_j   = crd(j,1:3)
    call pbc ( pos_i, pos_j )
    r_ijn   = pos_i-pos_j
    len_eta = length_vec(r_ijn)
    if ( len_eta <= cutoff ) then
      len_eta  = len_eta/eta
      f_scalar = (two_per_sqrtpi*dexp(-(len_eta**2))*len_eta &
               + erfc(len_eta))/(len_eta**3)
      f_r(i,1:3) = f_r(i,1:3) + charge(j)*f_scalar*r_ijn(1:3)
      f_r(j,1:3) = f_r(j,1:3) - charge(i)*f_scalar*r_ijn(1:3)
    endif
  enddo
enddo
per_eta_cube = 1.0d0/(eta**3)
do i=1,natom
  f_r(i,1:3)=f_r(i,1:3)*charge(i)*per_eta_cube
enddo
!
!   4. K-space Force
!

!
!     4-a. Initialize
!
f_k     = 0.0d0
pi2eta2 = pi**2 * eta**2
two_pi  = 2.0d0 * pi

i_max = int ( dsqrt(max_k2) )
do i_k=-i_max,i_max
  j_max = int ( dsqrt(max_k2-dble(i_k**2)) )
  do j_k=-j_max,j_max
    k_max = int ( dsqrt(max_k2-dble(i_k**2+j_k**2)) )
    do k_k=-k_max,k_max
      !write(0,*) 'i_k,j_k,k_k=',i_k,j_k,k_k
      if ( i_k == 0 .and. j_k == 0 .and. k_k == 0 ) then
        if ( idbg ) write(0,*) 'skipping due to (i,j,k)=(0,0,0).'
        cycle
      endif
      !
      !     4-b. Make k vector
      !
      !idbg = .true.
      k_b=dble(i_k)*basis_k(1,1:3)+ &
          dble(j_k)*basis_k(2,1:3)+ &
          dble(k_k)*basis_k(3,1:3)
      k_normsq=k_b(1)**2+k_b(2)**2+k_b(3)**2
      idbg=.false.
      !
      !     4-c. Make force
      !
      !idbg = .true.
      do i=1,natom
        k_br_i=k_b(1)*crd(i,1)+k_b(2)*crd(i,2)+k_b(3)*crd(i,3)
        do j=i+1,natom
          k_br_j=k_b(1)*crd(j,1)+k_b(2)*crd(j,2)+k_b(3)*crd(j,3)
          if ( idbg ) then
            write(0,'(a,i4,3f8.4)') 'iatom, crd = ', i, (crd(i,ixyz),ixyz=1,3)
            write(0,'(a,i4,3f8.4)') 'jatom, crd = ', j, (crd(j,ixyz),ixyz=1,3)
            write(0,'(a,3i4,4f8.4)')              &
              'i_k, j_k, k_k, k_br_i, charge(i), charge(j)=', &
               i_k, j_k, k_k, k_br_j, charge(i), charge(j)
          endif
            f_scalar   = dexp(-pi2eta2*k_normsq)/k_normsq*dsin(two_pi*(k_br_i-k_br_j))
            f_k(i,1:3) = f_k(i,1:3) + f_scalar * charge(j) * k_b(1:3)
            f_k(j,1:3) = f_k(j,1:3) - f_scalar * charge(i) * k_b(1:3)
        enddo
      enddo
      idbg = .false.
      !
    enddo
  enddo
enddo
!
!   4-d. Multiply factor to f_k
!
do i=1,natom
  f_k(i,1:3)=f_k(i,1:3)*2.0d0*charge(i)/vol
enddo
!
!   5. Force of Excluded Pairs
!
!idbg=.true.
f_ex=0.0d0
p_numex=1
do i=1,natom
  do i_numex=1,numex(i) 
    j=inb(p_numex)
    if ( j==0 ) exit
    pos_i=crd(i,1:3)
    pos_j=crd(j,1:3)
    call pbc ( pos_i, pos_j )
    len=length(pos_i,pos_j) 
    len_eta=length(pos_i,pos_j) / eta
    f_scalar = charge(i) * charge(j) / len**3 * &
      ( two_per_sqrtpi * len_eta * dexp(-len_eta**2) - erf(len_eta) )
    f_ex(i,1:3) = f_ex(i,1:3) + f_scalar * ( pos_i(1:3) - pos_j(1:3) )
    f_ex(j,1:3) = f_ex(j,1:3) - f_scalar * ( pos_i(1:3) - pos_j(1:3) )
    p_numex=p_numex+1
    if ( idbg ) write(0,*) 'ewald->excluded:i,j=',i,j
  enddo
enddo
idbg=.false.
!
!   6. Total Force
!
idbg = .true.
f_ewald = f_r + f_k - f_ex
if ( idbg ) then
  write(0,'(a)') ' Force of Ewald Sum'
  do i=1,natom
    write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (f_ewald(i,ixyz),ixyz=1,3)
  enddo
  write(0,'(a)') ' Force of R-Space'
  do i=1,natom
    write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (f_r(i,ixyz),ixyz=1,3)
  enddo
  write(0,'(a)') ' Force of K-Space'
  do i=1,natom
    write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (f_k(i,ixyz),ixyz=1,3)
  enddo
  write(0,'(a)') ' Force of Excluded Pairs'
  do i=1,natom
    write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (f_ex(i,ixyz),ixyz=1,3)
  enddo
endif
idbg = .false.

return
endsubroutine
