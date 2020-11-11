subroutine pme (tcrd, charge, natom, e_ewald, numex, inb, tf_tot )
!
!     This subroutine is based on "Particle mesh Ewald:
!   An N*log(N) method for Ewald sums in large systems"
!   T. Darden et. al., J. Chem. Phys., 98, 10089-10092.
!
implicit none
include 'size.inc'
include 'const.inc'
include 'mmpara.inc'
include 'gau.inc'
include 'pme.inc'
!include 'clock.inc'

integer, intent(in) :: natom
real*8,  intent(in) :: charge(m_natom)

real*8              :: vol                        ! cell volume:vol=cell(1)*cell(2)*cell(3)
            
integer             :: numex(m_natom)             ! API compatibility
integer             :: inb(m_nnb)                 ! API compatibility

real*8,intent(out)  :: e_ewald                    ! Total Energy
real*8              :: e_r                        ! R-space Coulombic energy
real*8              :: e_k                        ! K-space Coulombic energy
real*8              :: e_self                     ! self-interaction

real*8              :: tf_ewald(3,m_natom)        ! Integrated PME force
real*8              :: tf_r(3,m_natom)            ! R-space Coulomb force
real*8              :: tf_k(3,m_natom)            ! K-space Coulomb force

real*8              :: eta                        ! parameter for Ewald decomposition

real*8              :: tcrd(3, natom)             ! coordinates

integer             :: i, ixyz                    ! counter
logical, parameter  :: idbg = .false.             ! flag for debug-mode

real*8, external    :: det3                       ! calculate determinant of 3x3-matrix
real*8              :: tf_tot(3,m_natom)          ! Integrated PME force
intrinsic dabs,dsqrt
!----------------------------------------------------------------------
!
!   1. Check parameter
!
if ( .not. ( is_pbc .and. is_ewald ) ) return
if ( ( max ( largek1, largek2, largek3 ) < p_spl ) .or. ( mod ( p_spl, 2 ) == 1 ) ) then
  write(0,*) 'Interpolation order must be even number'
  write(0,*) 'and smaller than max ( largek1, largek2, largek3 ).'
  return
endif

call clock_push('PME')
!tcrd = transpose(crd)
call clock_look('get tcrd')
!----------------------------------------------------------------------
!
!   2. Make Parameter
!
if ( idbg ) write(0,*) '  Get into Make Parameter @ pme_e' 
vol = dabs(det3(basis_r))
call guess_eta(cutoff, min(largek1, largek2, largek3), vol**(1.0d0/3.0d0), eta)
call clock_look('get params')
!----------------------------------------------------------------------
!
!   3. calculation
!
call r_space(tf_r, natom, tcrd, eta, charge, e_r)
call k_space(basis_r, tcrd, p_spl, largek1, largek2, largek3, &
             natom, charge, eta, e_k, tf_k)
call self_interaction(charge, natom, eta, e_self)
!----------------------------------------------------------------------
!
!   4. Total Energy
!
if ( idbg ) write(0,*) '  Get into Make Total Energy @ pme_e' 
e_ewald = e_r + e_k + e_self
tf_ewald = tf_r + tf_k
if (idbg) then
  call dump_pme_enes(e_ewald, e_r, e_k, e_self, tf_ewald, tf_r, tf_k, natom)
endif

tf_tot = tf_tot + tf_ewald
call clock_look('Integration')
call clock_pop

return
endsubroutine

subroutine getbsq(p_spl,largek,bsq)
implicit none
include 'const.inc'
integer    , intent(in)  :: p_spl               ! Interpolation order
integer    , intent(in)  :: largek              ! Grid size
real*8     , intent(out) :: bsq(0:largek-1)     ! |b(m)|**2
integer                  :: m                   ! Counter for grid ( 1-dimension )
integer                  :: k                   ! Counter for interpolation order
complex*16               :: denom(0:largek-1)   ! Denominator
real*8                   :: fmi(1:p_spl-1)      ! Array of fm(k)
complex*16               :: omega(0:largek-1)   ! omega = exp(fact * i)
real*8                   :: prefactor           ! step for (1d-)reciprocal grid
real*8                   :: theta               ! (1d-)reciprocal grid
real*8     , external    :: fm
intrinsic dble, dimag, cmplx, dcos, dsin

prefactor = (pi + pi) / dble(largek)
do k = 1, p_spl - 1
  fmi(k) = fm(p_spl, dble(k))
enddo
do m = 0, largek - 1
  theta = prefactor * dble(m)
  omega(m) = cmplx(dcos(theta), dsin(theta))
enddo

denom(:) = 0.0d0
do k = p_spl - 1, 1, -1
  denom(:) = denom(:) * omega(:) + fmi(k)
enddo
bsq(:) = 1.0d0 / (dble(denom(:)) ** 2 + dimag(denom(:)) ** 2)
return
endsubroutine

subroutine calc_k_grad(largek1, largek2, largek3, charge, potential, f_k, &
                       fmi, fmj, fmk, p_spl, largek_axis, eff_grid)
implicit none
integer, intent(in)  :: p_spl                                           ! order of B-spline interpolation
integer, intent(in)  :: largek1, largek2, largek3
real*8 , intent(in)  :: charge                                          ! charge of current atom
real*8 , intent(in)  :: potential(0:largek1-1,0:largek2-1,0:largek3-1)  ! real part of largeq_ft
real*8 , intent(in)  :: fmi(0:p_spl-1), fmj(0:p_spl-1), fmk(0:p_spl-1)  ! fm's along each direction
integer, intent(in)  :: largek_axis                                     ! #gridpoints along which dk/du is calculated.
real*8 , intent(out) :: f_k                                             ! dk/du_i for current atom & direction

real*8               :: work_i, work_j, work_k                          ! current accumurated values
integer              :: eff_grid(0:p_spl-1, 3)                          ! support of fm_i(u)
integer              :: pi_k, pj_k, pk_k                                ! unconverted indice for the gridpoint
integer              :: i_k, j_k, k_k                                   ! converted index for the current gridpoint

work_k = 0.0d0
do pk_k = 0, p_spl - 1
  k_k = eff_grid(pk_k, 3)
  work_j = 0.0d0
  do pj_k = 0, p_spl - 1
    j_k = eff_grid(pj_k, 2)
    work_i = 0.0d0
    do pi_k = 0, p_spl - 1
      i_k = eff_grid(pi_k, 1)
      work_i = work_i + fmi(pi_k) * potential(i_k, j_k, k_k)
    enddo
    work_j = work_j + fmj(pj_k) * work_i
  enddo
  work_k = work_k + fmk(pk_k) * work_j
enddo

! bug fixed (2017.08.13) largek1 -> largek_axis
! dr/du was moved to caller subroutine.
f_k = charge * dble(largek_axis) * work_k

return
endsubroutine

subroutine get_eff_grid(p_spl, tfcrd, largek1, eff_grid)
implicit none
integer             :: p_spl, pi_k, j, largek1
real*8              :: tfcrd
integer             :: eff_grid(0:p_spl-1)
integer             :: shift                   ! ensure eff_grid(pi_k) > 0
shift = ishft(largek1, 16)                     ! largek1 * enough_large_integer
do pi_k = 0, p_spl - 1
  eff_grid(pi_k) = mod(nint(tfcrd * dble(largek1) - 0.5d0) + shift - pi_k, largek1)
enddo
return
endsubroutine

subroutine get_largeq(p_spl, largek1, largek2, largek3, natom, charge, tfcrd, largeq)
implicit none
logical, parameter   :: idbg = .false.
integer, intent(in)  :: p_spl, largek1, largek2, largek3, natom
real*8 , intent(in)  :: charge(*), tfcrd(3,*)            ! fractional coordinates
real*8 , intent(out) :: largeq(0:largek1-1,0:largek2-1,0:largek3-1)  ! charge on grid point
integer              :: j, p_k, pi_k, pj_k, pk_k, i_k, j_k, k_k
real*8               :: fmi(0:p_spl-1), fmj(0:p_spl-1), fmk(0:p_spl-1)
real*8               :: work_i, work_j, work_k
integer              :: eff_grid(0:p_spl-1, 3)
if (idbg) write(0,*) '    Get into Get Charge on Grid Point @ pme_e' 
largeq = 0.0d0
do j = 1, natom
  call get_fm(tfcrd(1, j), largek1, p_spl, fmi)
  call get_fm(tfcrd(2, j), largek2, p_spl, fmj)
  call get_fm(tfcrd(3, j), largek3, p_spl, fmk)

  call get_eff_grid(p_spl, tfcrd(1, j), largek1, eff_grid(0:p_spl-1, 1))
  call get_eff_grid(p_spl, tfcrd(2, j), largek2, eff_grid(0:p_spl-1, 2))
  call get_eff_grid(p_spl, tfcrd(3, j), largek3, eff_grid(0:p_spl-1, 3))
  do pk_k = 0, p_spl - 1
    k_k = eff_grid(pk_k, 3)
    work_k = charge(j) * fmk(pk_k)
    do pj_k = 0, p_spl - 1
      j_k = eff_grid(pj_k, 2)
      work_j = work_k * fmj(pj_k)
      do pi_k = 0, p_spl - 1
        i_k = eff_grid(pi_k, 1)
        work_i = work_j * fmi(pi_k)
        largeq(i_k, j_k, k_k) = largeq(i_k, j_k, k_k) + work_i
      enddo
    enddo
  enddo
enddo
call clock_look('get largeq') 
return
endsubroutine

subroutine get_largephi(eta, largek1, largek2, largek3, basis_k, bsq1, bsq2, bsq3, largephi)
implicit none
include 'const.inc'
logical, parameter  :: idbg = .false.
real*8 , intent(in) :: basis_k(3,3)
real*8              :: eta                        ! parameter
integer, intent(in) :: largek1, largek2, largek3
real*8              :: bsq1(0:largek1-1),bsq2(0:largek2-1),bsq3(0:largek3-1) ! |b(m)|^2
integer             :: i_k, j_k, k_k, ixyz, jxyz
real*8              :: largephi(0:largek1-1,0:largek2-1,0:largek3-1)         ! (1/pi*V)sigma_(m/=0)[exp(-pi^2*|m|^2*eta^2)/|m|^2*
                                                                             ! exp[2*pi*i(m1f1+m2f2+m3f3)
                                                                             ! In original report, beta ( = 1 / eta ) is used 
                                                                             ! instead of eta.
                                                                             ! In this program, imaginary part is neglected
                                                                             ! because it is cancelled by summing case -m and m.
real*8              :: exptab(0:largek1-1,0:largek2-1,0:largek3-1)           ! exp(-pi^2*g^ij*eta^2*g^ij)^n.

real*8              :: k_b(3)                                                ! dble(i, j, k)
real*8              :: k_normsq                                              ! k_b_i k_b_j * g^ij

real*8              :: g_inv(3,3), kernel(3, 3, 0: max(largek1,largek2,largek3))
real*8              :: work_i, work_j, work_k
integer             :: i
real*8              :: kvol = 1.0d0
real*8 , external   :: det3

call clock_push('largephi')
if ( idbg ) write(0,*) '    Get into Get largephi @ pme_e' 
g_inv = matmul(basis_k, transpose(basis_k))
kernel(1:3, 1:3, 0) = 1.0d0
kernel(1:3, 1:3, 1) = dexp(-(pi * eta)**2 * g_inv)

call clock_look('setup')

do i = 2, max(largek1,largek2,largek3)
  kernel(1:3, 1:3, i) = kernel(1:3, 1:3, i-1) * kernel(1:3, 1:3, 1)
enddo
call clock_look('kernel')

! case :i_k = 0, j_k = 0, k_k = 0
i_k = 0
j_k = 0
k_k = 0
exptab(i_k, j_k, k_k) = 1.0d0
largephi(i_k, j_k, k_k) = 0.0d0
call clock_look('k_k=0')

! case :i_k = 0, j_k = 0, k_k > 0
work_i = bsq1(i_k) 
work_j = bsq2(j_k) * work_i
do k_k = 1, largek3 / 2
  work_k = bsq3(k_k) * work_j
  k_b = dble((/i_k, j_k, k_k/))
  k_normsq = dot_product(k_b, matmul(g_inv, k_b))
  exptab(i_k, j_k, k_k) = exptab(i_k, j_k, k_k-1) &
    * kernel(3,3, 2*k_k-1)
  largephi ( i_k, j_k, k_k ) = exptab(i_k, j_k, k_k) &
    / k_normsq * work_k
enddo
call clock_look('j_k=0')

! case: i_k = 0, j_k > 0, k_k > 0
work_i = bsq1(i_k) 
do j_k = 1, largek2 / 2
  work_j = bsq2(j_k) * work_i
  do k_k = 0, largek3 / 2
    work_k = bsq3(k_k) * work_j
    k_b = dble((/i_k, j_k, k_k/))
    k_normsq = dot_product(k_b, matmul(g_inv, k_b))
    exptab(i_k, j_k, k_k) = exptab(i_k, j_k-1, k_k) &
      * kernel(2,2, 2*j_k-1) * kernel(3,2, 2*k_k)
    largephi ( i_k, j_k, k_k ) = exptab(i_k, j_k, k_k) &
      / k_normsq * work_k
  enddo
enddo
call clock_look('i_k=0')

! case: i_k > 0, j_k > 0, k_k > 0
do i_k = 1, largek1 / 2
  work_i = bsq1(i_k) 
  do j_k = 0, largek2 / 2
    work_j = bsq2(j_k) * work_i
    do k_k = 0, largek3 / 2
      work_k = bsq3(k_k) * work_j
      k_b = dble((/i_k, j_k, k_k/))
      k_normsq = dot_product(k_b, matmul(g_inv, k_b))
      exptab(i_k, j_k, k_k) = exptab(i_k-1, j_k, k_k) &
        * kernel(1,1, 2*i_k-1) * kernel(2,1, 2*j_k) * kernel(3,1, 2*k_k)
      largephi ( i_k, j_k, k_k ) = exptab(i_k, j_k, k_k) &
        / k_normsq * work_k
    enddo
  enddo
enddo
call clock_look('i>k=0')

kvol = dabs(det3(basis_k))
largephi(0:largek1/2, 0:largek2/2, 0:largek3/2) = largephi(0:largek1/2, 0:largek2/2, 0:largek3/2) * (kvol / pi)
call clock_look('division')
largephi(largek1-1:largek1-(largek1/2):-1, 0:largek2/2, 0:largek3/2) = largephi(1:(largek1 / 2), 0:largek2/2, 0:largek3/2)
largephi(:, largek2-1:largek2-(largek2/2):-1, 0:largek3/2) = largephi(:, 1:(largek2 / 2), 0:largek3/2)
largephi(:, :, largek3-1:largek3-(largek3/2):-1) = largephi(:, :, 1:(largek2 / 2))
call clock_look('mirror')
call clock_pop
return
endsubroutine

subroutine get_fm(tfcrd, largekx, p_spl, fmx)
implicit none
real*8 , intent(in)  :: tfcrd
integer, intent(in)  :: largekx, p_spl
real*8 , intent(out) :: fmx(0: p_spl - 1)
integer              :: i
real*8               :: u_base
real*8 , external    :: fm
u_base = tfcrd * dble(largekx)
u_base = u_base - dble(dnint(u_base - 0.5d0))
do i = 0, p_spl - 1
  fmx(i) = fm(p_spl, u_base+dble(i))
enddo
return
endsubroutine

subroutine get_fm_dfm(tfcrd, largekx, p_spl, fmx, dfmx)
implicit none
real*8 , intent(in)  :: tfcrd
integer, intent(in)  :: largekx, p_spl
real*8 , intent(out) :: fmx(0: p_spl - 1), dfmx(0: p_spl - 1)
integer              :: i
real*8               :: u_base
real*8 , external    :: fm
u_base = tfcrd * dble(largekx)
u_base = u_base - dble(dnint(u_base - 0.5d0))
do i = 0, p_spl - 1
  fmx(i) = fm(p_spl, u_base+dble(i))
enddo
do i = 0, p_spl - 2
  dfmx(i) = fm(p_spl - 1, u_base+dble(i))
enddo
dfmx(p_spl - 1) = 0.0d0
do i = p_spl - 1, 1, -1
  dfmx(i) = dfmx(i) - dfmx(i - 1)
enddo
return
endsubroutine

subroutine get_f_k(natom, tfcrd, largek1, largek2, largek3, p_spl, charge, basis_k, potential, tf_k)
! This subroutine was made to correspond to parallelpiped cell,
! but has not been tested in non-orthogonal cell.
implicit none
integer, intent(in)  :: natom
real*8 , intent(in)  :: tfcrd(3, *)
integer, intent(in)  :: largek1, largek2, largek3
integer, intent(in)  :: p_spl
real*8 , intent(in)  :: charge(*)
real*8 , intent(in)  :: basis_k(3, 3)
real*8 , intent(in)  :: potential(0:largek1-1,0:largek2-1,0:largek3-1)  ! real part of largeq_ft
real*8               :: tf_k(3, *)
real*8               :: fmi(0:p_spl-1), fmj(0:p_spl-1), fmk(0:p_spl-1)
real*8               :: dfmi(0:p_spl-1), dfmj(0:p_spl-1), dfmk(0:p_spl-1)
integer              :: eff_grid(0:p_spl-1, 3)
integer              :: j
tf_k(:, 1:natom) = 0.0d0
do j = 1, natom
  call get_fm_dfm(tfcrd(1, j), largek1, p_spl, fmi, dfmi)
  call get_fm_dfm(tfcrd(2, j), largek2, p_spl, fmj, dfmj)
  call get_fm_dfm(tfcrd(3, j), largek3, p_spl, fmk, dfmk)
  call get_eff_grid(p_spl, tfcrd(1, j), largek1, eff_grid(0:p_spl-1, 1))
  call get_eff_grid(p_spl, tfcrd(2, j), largek2, eff_grid(0:p_spl-1, 2))
  call get_eff_grid(p_spl, tfcrd(3, j), largek3, eff_grid(0:p_spl-1, 3))
  call calc_k_grad(largek1, largek2, largek3, &
    charge(j), potential, tf_k(1, j), dfmi, fmj, fmk, p_spl, largek1, eff_grid) !   x direction
  call calc_k_grad(largek1, largek2, largek3, &
    charge(j), potential, tf_k(2, j), fmi, dfmj, fmk, p_spl, largek2, eff_grid) !   y direction
  call calc_k_grad(largek1, largek2, largek3, &
    charge(j), potential, tf_k(3, j), fmi, fmj, dfmk, p_spl, largek3, eff_grid) !   z direction
enddo
! Since u = largek_axis * frac.crd., d/dx = largek_axis * (basis_k)^t * d/du.
tf_k(:, 1:natom) = matmul(transpose(basis_k), tf_k(:, 1:natom))
tf_k(:, 1:natom) = -tf_k(:, 1:natom) * dble(largek1 * largek2 * largek3)
call clock_look('get f_k') 
return
endsubroutine

subroutine get_potential(largeq, largephi, largek1, largek2, largek3, potential)
implicit none
include 'fftw3.f'
integer, intent(in)  :: largek1, largek2, largek3
real*8 , intent(in)  :: largeq(0:largek1-1, 0:largek2-1, 0:largek3-1)
real*8 , intent(in)  :: largephi(0:largek1-1, 0:largek2-1, 0:largek3-1)
real*8 , intent(out) :: potential(0:largek1-1, 0:largek2-1, 0:largek3-1)
complex*16           :: potential_cmplx(0:largek1-1, 0:largek2-1, 0:largek3-1)
integer*8            :: fft_plan
potential_cmplx = dcmplx(largeq)
call dfftw_plan_dft_3d ( fft_plan, largek1, largek2, largek3, &
  potential_cmplx, potential_cmplx, FFTW_FORWARD, FFTW_ESTIMATE)
call clock_look('after_plan')
call dfftw_execute_dft(fft_plan, potential_cmplx, potential_cmplx)
call clock_look('execute_dft(forward)')
call dfftw_destroy_plan(fft_plan)
call clock_look('destroy_plan')
potential_cmplx = largephi * potential_cmplx
call dfftw_plan_dft_3d ( fft_plan, largek1, largek2, largek3, &
  potential_cmplx, potential_cmplx, FFTW_BACKWARD, FFTW_ESTIMATE)
call clock_look('after_plan')
call dfftw_execute_dft(fft_plan, potential_cmplx, potential_cmplx)
call clock_look('execute_dft(backward)')
call dfftw_destroy_plan(fft_plan)
call clock_look('destroy_plan')
potential = dble ( potential_cmplx ) / dble(largek1 * largek2 * largek3)
call clock_look('get potential') 
return
endsubroutine

subroutine getbsq123(p_spl, largek1, largek2, largek3, bsq1, bsq2, bsq3)
implicit none
integer, intent(in)  :: p_spl, largek1, largek2, largek3
real*8 , intent(out) :: bsq1(0:largek1-1),bsq2(0:largek2-1),bsq3(0:largek3-1) ! |b(m)|^2
call getbsq ( p_spl, largek1, bsq1 )
call getbsq ( p_spl, largek2, bsq2 )
call getbsq ( p_spl, largek3, bsq3 )
call clock_look('get bsq') 
return
endsubroutine

subroutine get_e_k(largeq, potential, largek1, largek2, largek3, e_k)
real*8               :: largeq(0:largek1-1, 0:largek2-1, 0:largek3-1)
real*8               :: potential(0:largek1-1, 0:largek2-1, 0:largek3-1)
integer, intent(in)  :: largek1, largek2, largek3
real*8 , intent(out) :: e_k
largeq = largeq * potential
e_k = 0.5d0 * sum(largeq) * dble (largek1 * largek2 * largek3)
call clock_look('get e_k') 
return
endsubroutine

subroutine get_tfcrd(basis_k, tcrd, tfcrd, natom)
include 'size.inc'
real*8, intent(in)  :: basis_k(3, 3), tcrd(3, *)
integer, intent(in) :: natom
real*8, intent(out) :: tfcrd(3, *)
tfcrd(:, 1:natom) = matmul(basis_k, tcrd(:, 1:natom))
call clock_look('get tfcrd') 
endsubroutine

subroutine k_space(basis_r, tcrd, p_spl, largek1, largek2, largek3, &
                   natom, charge, eta, e_k, tf_k)
implicit none
integer             :: largek1, largek2, largek3, p_spl
integer             :: natom
real*8              :: basis_r(3,3), basis_k(3,3), tcrd(3, *), charge(*)
real*8              :: eta, vol
real*8              :: e_k, tf_k(3, *)
real*8              :: bsq1(0:largek1-1),bsq2(0:largek2-1),bsq3(0:largek3-1) ! |b(m)|^2
real*8              :: largeq   (0:largek1-1,0:largek2-1,0:largek3-1)        ! charge on grid point
real*8              :: potential(0:largek1-1,0:largek2-1,0:largek3-1)        ! real part of largeq_ft
real*8              :: largephi (0:largek1-1,0:largek2-1,0:largek3-1)        ! (1/pi*V)sigma_(m/=0)[exp(-pi^2*|m|^2*eta^2)/|m|^2*
                                                                             ! exp[2*pi*i(m1f1+m2f2+m3f3)
                                                                             ! In original report, beta ( = 1 / eta ) is used 
                                                                             ! instead of eta.
                                                                             ! In this program, imaginary part is neglected
                                                                             ! because it is cancelled by summing case -m and m.
real*8              :: tfcrd(3, natom)            ! fractional coordinates

call clock_push('k-space')
call setup_fm
call get_basis_k(basis_r, basis_k)
call get_tfcrd(basis_k, tcrd, tfcrd, natom)
call getbsq123(p_spl, largek1, largek2, largek3, bsq1, bsq2, bsq3)
call get_largeq(p_spl, largek1, largek2, largek3, natom, charge, tfcrd, largeq)
call get_largephi(eta, largek1, largek2, largek3, basis_k, bsq1, bsq2, bsq3, largephi)
call get_potential(largeq, largephi, largek1, largek2, largek3, potential)
call get_e_k(largeq, potential, largek1, largek2, largek3, e_k)
call get_f_k(natom, tfcrd, largek1, largek2, largek3, p_spl, charge, basis_k, potential, tf_k)
call clock_pop
endsubroutine

subroutine get_basis_k(basis_r, basis_k)
implicit none
integer, parameter :: n = 3, lwork = n * n
real*8             :: basis_r(n, n), basis_k(n, n), work(lwork)
integer            :: ipiv(n), info
basis_k = basis_r
call dgetrf(n, n, basis_k, n, ipiv, info)
call dgetri(n, basis_k, n, ipiv, work, lwork, info)
call clock_look('get_basis_k')
endsubroutine

subroutine self_interaction(charge, natom, eta, e_self)
implicit none
include 'const.inc'
logical, parameter   :: idbg = .false.
real*8 , intent(in)  :: charge(*)
real*8 , intent(in)  :: eta
integer, intent(in)  :: natom
real*8 , intent(out) :: e_self
integer              :: i
!idbg = .true.
if (idbg) write(0,*) '  Get into Make Self Interaction @ pme_e' 
e_self = 0.0d0
do i=1,natom
  e_self=e_self+charge(i)**2
enddo
if (idbg) write(0,*) '  Sum(q^2), eta = ', e_self, eta
e_self = -e_self / (eta * dsqrt(pi))
call clock_look('Self_interaction')
endsubroutine

subroutine r_space(tf_r, natom, tcrd, eta, charge, e_r)
implicit none
include 'const.inc'
include 'mmpara.inc'
logical, parameter  :: idbg = .false.
real*8              :: e_r                        ! R-space Coulomb energy
real*8              :: e_r_tmp                    ! Temporary variable to R-space Coulomb energy
real*8              :: tf_r(3, *)                 ! R-space Coulomb force
real*8              :: r_ijn(3)                   ! r_i-r_j ( minimum image )
real*8              :: two_per_sqrtpi             ! 2/sqrt(pi)
integer, intent(in) :: natom
real*8,  intent(in) :: charge(*)
real*8              :: tcrd(3, *)
real*8              :: eta                        ! parameter
real*8              :: per_eta                    ! 1/eta
real*8              :: len                        ! distance between atom i and j
real*8              :: f_scalar                   ! Temporaly force
real*8              :: len_eta                    ! len/eta
real*8              :: per_eta_cube               ! eta^(-3)
real*8              :: erfc_len_eta
real*8,external     :: length,length_vec,fm

integer             :: i, j
if ( idbg ) write(0,*) '  Get into Make R-space energy @ pme_e' 
if ( idbg ) write(0,*) '    cutoff=',cutoff
e_r = 0.0d0
tf_r(:, 1:natom) = 0.0d0
two_per_sqrtpi = 2.0d0 / dsqrt(pi)
per_eta = 1.0d0 / eta
do i=1,natom
  e_r_tmp=0.0d0
  do j=i+1,natom
    r_ijn = tcrd(:, i) - tcrd(:, j)
    call pbc_parallelpiped(r_ijn, basis_r, basis_k)
    len = length_vec ( r_ijn )
    if ( len > cutoff ) cycle
    len_eta = len * per_eta
    erfc_len_eta = erfc(len_eta)
    e_r_tmp = e_r_tmp + charge(j) / len * erfc_len_eta
    f_scalar = (two_per_sqrtpi * dexp(-len_eta * len_eta) * len_eta &
             + erfc_len_eta) / ( len_eta * len_eta * len_eta)
    tf_r(1:3, i) = tf_r(1:3, i) + charge(j) * f_scalar * r_ijn(1:3)
    tf_r(1:3, j) = tf_r(1:3, j) - charge(i) * f_scalar * r_ijn(1:3)
  enddo
  e_r = e_r + e_r_tmp * charge(i)
enddo

per_eta_cube = 1.0d0 / ( eta**3 )
do i=1,natom
  tf_r(1:3, i) = tf_r(1:3, i) * charge(i) * per_eta_cube
enddo
call clock_look('R_space')
return
endsubroutine

subroutine dump_pme_enes(e_ewald, e_r, e_k, e_self, tf_ewald, tf_r, tf_k, natom)
implicit none
include 'size.inc'
real*8, intent(in)  :: e_ewald                    ! Total Energy
real*8, intent(in)  :: e_r                        ! R-space Coulomb energy
real*8, intent(in)  :: e_k                        ! K-space Coulomb energy
real*8, intent(in)  :: e_self                     ! self-interaction

real*8, intent(in)  :: tf_ewald(3, *)             ! Force
real*8, intent(in)  :: tf_r(3, *)                 ! R-space Coulomb force
real*8, intent(in)  :: tf_k(3, *)                 ! K-space Coulomb force

integer             :: i, ixyz
integer             :: natom
write(0,'(a,f20.12)') ' Energy of Ewald Sum  = ', e_ewald
write(0,'(a,f20.12)') '   R-Space Energy     = ', e_r
write(0,'(a,f20.12)') '   K-Space Energy     = ', e_k
write(0,'(a,f20.12)') '   Self Interaction   = ', e_self

write(0,'(a)') ' Force of Ewald Sum'
do i=1,natom
  write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (tf_ewald(ixyz, i),ixyz=1,3)
enddo
write(0,'(a)') ' Force of R-Space'
do i=1,natom
  write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (tf_r(ixyz, i),ixyz=1,3)
enddo
write(0,'(a)') ' Force of K-Space'
do i=1,natom
  write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (tf_k(ixyz, i),ixyz=1,3)
enddo
endsubroutine

subroutine guess_eta(cutoff, largek, width, eta)
implicit none
include 'const.inc'
include 'pme.inc'
real*8 , intent(in)  :: cutoff, width
integer, intent(in)  :: largek
real*8 , intent(out) :: eta
real*8               :: eta_old
integer              :: i
real*8 , parameter   :: thresh = 1.0d-6
integer, parameter   :: max_iter = 100
real*8               :: pme_coeff
real*8               :: r_err, k_err, p_err
real*8               :: dr_err, dk_err, dp_err
real*8 , parameter   :: wr_err = 1.0d0, wk_err = 1.0d0, wp_err = 1.0d-10
real*8 , parameter   :: e = 2.718281828d0
logical, parameter   :: idbg = .false.
eta = 3.0d0
! By the requirement of PME, p_spl must be even.
! Therefore, p_spl / 2 gives the correct value.
pme_coeff &
  = (width / (dble(largek) * pi)) ** (p_spl) &
  * (dble(p_spl-1)/e) ** (p_spl-1) &
  / (dble(p_spl/2-1)/e) ** (p_spl/2-1) &
  / dsqrt(2.0d0*pi) * (18.2223d0)**2
! conversion criteria: r_err = (k_err + p_err)
do i = 1, max_iter
  eta_old = eta
  r_err = derfc(cutoff / eta) / cutoff
  k_err = dexp(-(pi * eta * largek / width) ** 2) / largek ** 2
  p_err = (pme_coeff * eta**(-p_spl-1))
  dr_err = (1.0d0 / (eta ** 2) * 2 / dsqrt(pi) * dexp(-(cutoff / eta) ** 2))
  dk_err = (-2 * eta * (pi / width) ** 2 * dexp(-(pi * eta * largek / width) ** 2))
  dp_err = (-p_spl - 1) * pme_coeff * eta ** (-p_spl-2)
  eta = eta - (k_err * wk_err + p_err * wp_err - r_err * wr_err) &
            / (dk_err * wr_err + dp_err * wp_err - dr_err * wr_err)
  if (dabs(eta_old - eta) < thresh) exit
enddo
eta = min(max(eta, 1.0d-2), 1.0d2)
r_err = derfc(cutoff / eta) / cutoff
k_err = dexp(-(pi * eta * largek / width) ** 2) / largek ** 2
p_err = (pme_coeff * eta**(-p_spl-1))
write(*, *) '<< eta estimation >>'
write(*, *) 'eta', eta
write(*, *) 'r_err', r_err, '(direct)', r_err * wr_err, '(scaled)'
write(*, *) 'k_err', k_err, '(direct)', k_err * wk_err, '(scaled)'
write(*, *) 'p_err', p_err, '(direct)', p_err * wp_err, '(scaled)'
return
endsubroutine
