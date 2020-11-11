subroutine ewald_e ( charge, natom, e_ewald, numex, inb )
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

real*8,intent(out) :: e_ewald            ! Total Energy
real*8             :: e_r                ! R-space Coulomb energy
real*8             :: e_r_tmp            ! Temporary variable to R-space Coulomb energy
real*8             :: e_k                ! K-space Coulomb energy
real*8             :: e_self             ! self-interaction
real*8             :: e_ex               ! energy of excluded pairs

real*8             :: pos_i(3)           ! position of atom i
real*8             :: pos_j(3)           ! minimum image of crd(j,1:3)
real*8             :: len                ! distance between atom i and j

integer            :: i_k,j_k,k_k        ! counter to box
real*8             :: k_b(3)             ! k/B
real*8             :: k_br               ! dot_product(k_b,r(i))
real*8             :: k_normsq           ! | k/b |
real*8             :: vol                ! cell volume:vol=cell(1)*cell(2)*cell(3)
!real*8             :: basis_r(3,3)       ! basis set in real space
!real*8             :: basis_k(3,3)       ! basis set in reciprocal space

real*8             :: eta                ! parameter
real*8             :: max_k2             ! k cutoff if ( |k|^2 > max_k2 )

real*8             :: s_cos,s_sin        ! sum of cosine part and sine part
integer            :: i_max,j_max,k_max  ! k cutoff by element of k_vector

integer            :: i_numex            ! counter of numex
integer            :: p_numex            ! pointer
real*8,external    :: length,erfc,erf

intrinsic dot_product,dcos,dsin,dexp,dabs,dsqrt,int,dble

!****************************************************************************!
!                                                                            !
!           1        [      {              (  q_j        (r_ijn ) )}]        !
!    e_r = --- sigma [ sigma{ q_I   sigma  ( ----- * erfc(----- ) )}]        !
!           2    n   [   I  {     j/=I,n/=0( r_ijn       ( eta  ) )}]        !
!                                                                            !
!                                                                            !
!             1         [ exp[(-pi^2*k^2)/(eta^2*B^2)]                       !
!    e_k = ------ sigma [ ----------------------------                       !
!          2*pi*V   k   [           (k/B)^2                                  !
!                                                                            !
!          * { ( sigma_I q_I*cos(2*pi(k/B)*r_I) )^2                          !
!                                                                            !
!            + ( sigma_I q_I*sin(2*pi(k/B)*r_I) )^2  } ]                     !
!                                                                            !
!    e_self = [-1/(eta*sqrt(pi))]*sigma_I[q_I^2]                             !
!                                                                            !
!    V          : Volume of unit cell                                        !
!    eta        : parameter ( arbitrary,but decide after paragraph )         !
!    n(vector)  : integer vector in real space                               !
!    k(vector)  : integer vector in reciprocal space                         !
!    B          : box size                                                   !
!    k/B        : reciprocal lattice vector                                  !
!    r_I(vector): atom position ( origin is arbitrary )                      !
!    q_I        : charge ( to put coefficient 1, divided by 18.3332 )        !
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

e_ewald = 0.0d0
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
idbg = .true.
eta     = cutoff / dsqrt(p)
max_k2  = ( ( vol**0.33333333d0 * cutoff ) / ( pi * eta**2 ) ) ** 2
if ( idbg ) write(0,'(a,2f12.4,i6)') 'eta,max_k= ', eta, dsqrt(max_k2)
idbg= .false.
!
!   3. R-space energy
!
idbg=.true.
e_r = 0.0d0
write(0,*) 'cutoff=',cutoff
do i=1,natom
  pos_i=crd(i,1:3)
  e_r_tmp=0.0d0
  do j=i+1,natom
    pos_j=crd(j,1:3)
    call pbc ( pos_i, pos_j )
    len=length(pos_i,pos_j)
    if ( len <= cutoff ) then
      e_r_tmp=e_r_tmp+charge(j)/len*erfc(len/eta)
    endif
  enddo
  e_r=e_r+e_r_tmp*charge(i)
enddo
idbg = .false.
!
!   4. K-space energy
!
e_k=0.0d0
i_max = int ( dsqrt(max_k2) )
do i_k=-i_max,i_max
  j_max = int ( dsqrt(max_k2-dble(i_k**2)) )
  do j_k=-j_max,j_max
    k_max = int ( dsqrt(max_k2-dble(i_k**2+j_k**2)) )
    do k_k=-k_max,k_max
      if ( i_k == 0 .and. j_k == 0 .and. k_k == 0 ) then
        if ( idbg ) write(0,*) 'skipping due to (i,j,k)=(0,0,0).'
        cycle
      else
        if ( idbg ) write(0,*) 'i_max,j_max,k_max=', i_max,j_max,k_max
      endif
      !
      !   4-a. Initialize
      !
      s_cos=0.0d0
      s_sin=0.0d0
      k_b=dble(i_k)*basis_k(1,1:3)+ &
          dble(j_k)*basis_k(2,1:3)+ &
          dble(k_k)*basis_k(3,1:3)
      k_normsq=k_b(1)**2+k_b(2)**2+k_b(3)**2
      !
      !   4-b. Sum over iatom
      !
      do i=1,natom
        k_br=k_b(1)*crd(i,1)+k_b(2)*crd(i,2)+k_b(3)*crd(i,3)
        s_cos=s_cos+charge(i)*dcos(2.0d0*pi*k_br)
        s_sin=s_sin+charge(i)*dsin(2.0d0*pi*k_br)
        if ( idbg ) then
          write(0,'(a,i4,3f8.4)') 'iatom, crd = ', i, (crd(i,ixyz),ixyz=1,3)
          write(0,'(a,3i4,4f8.4)')              &
            'i_k, j_k, k_k, k_br, charge(i), s_cos, s_sin=', &
             i_k, j_k, k_k, k_br, charge(i), s_cos, s_sin
        endif
      enddo
      !
      !   4-c. Sum for e_k
      !
      e_k=e_k+dexp(-pi**2*eta**2*k_normsq)*(s_cos**2+s_sin**2)/k_normsq
      if ( idbg ) then
        write(0,'(a,f20.12)') 'E(K-space)', e_k
      endif
      !
    enddo
  enddo
enddo
!
!   4-d. Multiply Factor to e_k
!
!idbg=.true.
e_k=e_k/(2.0d0*pi*vol)
if ( idbg ) write(0,*) 'e_k=',e_k
!
!   5. Self Interaction
!
e_self = 0.0d0
do i=1,natom
  e_self=e_self+charge(i)**2
enddo
e_self=-e_self/(eta*dsqrt(pi))
!
!   6. Excluded Atoms
!
!idbg=.true.
e_ex=0.0d0
p_numex=1
do i=1,natom
  do i_numex=1,numex(i) 
    j=inb(p_numex)
    if ( j==0 ) exit
    pos_i=crd(i,1:3)
    pos_j=crd(j,1:3)
    call pbc ( pos_i, pos_j )
    len=length(pos_i,pos_j)
    e_ex=e_ex+charge(i)*charge(j)/len*erf(len/eta)
    p_numex=p_numex+1
    if ( idbg ) write(0,*) 'ewald->excluded:i,j=',i,j
  enddo
enddo
!idbg=.false.
!
!   7. Total Energy
!
idbg = .true.
e_ewald = e_r + e_k + e_self - e_ex
if ( idbg ) then
  write(0,'(a,f20.12)') ' Energy of Ewald Sum  = ', e_ewald
  write(0,'(a,f20.12)') '   R-Space Energy     = ', e_r
  write(0,'(a,f20.12)') '   K-Space Energy     = ', e_k
  write(0,'(a,f20.12)') '   Self Interaction   = ', e_self
  write(0,'(a,f20.12)') '   Excluded Pairs     = ', e_ex
endif
!idbg = .false.

return
endsubroutine
