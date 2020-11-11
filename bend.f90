subroutine bend ( tcrd, tk, teq, e_bend, tf_bend, nthet, ijkict_tmp )
implicit none
include 'const.inc'
include 'size.inc'
include 'gau.inc'
include 'mmpara.inc'
real*8 , intent(out) :: e_bend
real*8 , intent(out) :: tf_bend(3, m_natom)
integer, intent(in)  :: nthet
integer, intent(in)  :: ijkict_tmp(*)

real*8 , intent(in)  :: tcrd(3, *)
integer              :: atom_i,atom_j,atom_k
integer              :: indlist

real*8               :: TK(m_numang),   TEQ(m_numang)

real*8               :: rij(3),rkj(3)
real*8               :: theta, delta_theta, cos_theta
real*8               :: scalar, dijkj, dijij, dkjkj, pijkj
real*8               :: f_i(3),f_k(3)

integer              :: bend_n
integer              :: bend_is(m_ntheta+m_ntheth)
integer              :: bend_js(m_ntheta+m_ntheth)
integer              :: bend_ks(m_ntheta+m_ntheth)
real*8               :: bend_teqs(m_ntheta+m_ntheth)
real*8               :: bend_tks(m_ntheta+m_ntheth)
real*8               :: bend_teq, bend_tk

integer       :: i,j,k!,l,ixyz
logical, parameter :: idbg = .false.
!integer cnt
intrinsic dcos, dsqrt, dot_product, dacos

call clock_push('angle')
if (idbg) write(0,*) 'nthet = ',nthet

! generate mmlist
bend_n = 0
do i=1,nthet
  atom_i = ijkict_tmp(4*i-3)/3+1
  atom_j = ijkict_tmp(4*i-2)/3+1
  atom_k = ijkict_tmp(4*i-1)/3+1
  if ( atom_layer ( atom_i ) .or. &
       atom_layer ( atom_j ) .or. &
       atom_layer ( atom_k ) ) then
    if ( idbg ) then
      write(0,'(2a10,4x,3i4,a18)') 'Bend_e:','Angle',atom_i,atom_j,atom_k,' was skipped.'
    endif
    cycle
  endif
  indlist = ijkict_tmp(4*i)
  bend_n = bend_n + 1
  bend_is(bend_n)   = ijkict_tmp(4*i-3)/3+1
  bend_js(bend_n)   = ijkict_tmp(4*i-2)/3+1
  bend_ks(bend_n)   = ijkict_tmp(4*i-1)/3+1
  bend_teqs(bend_n) = teq(indlist)
  bend_tks(bend_n)  = tk(indlist)
enddo

e_bend = 0.0d0
!tf_bend = 0.0d0
do i = 1, bend_n
  atom_i   = bend_is(i)   
  atom_j   = bend_js(i)   
  atom_k   = bend_ks(i)   
  bend_teq = bend_teqs(i) 
  bend_tk  = bend_tks(i)  
  
  rij = tcrd(1:3, atom_j) - tcrd(1:3, atom_i)
  rkj = tcrd(1:3, atom_j) - tcrd(1:3, atom_k)
  if (is_pbc) then
    call pbc_parallelpiped(rij, basis_r, basis_k)
    call pbc_parallelpiped(rkj, basis_r, basis_k)
  endif
  dijij = dot_product(rij, rij)
  dkjkj = dot_product(rkj, rkj)
  dijkj = dot_product(rij, rkj)
  pijkj = dsqrt(dijij * dkjkj)
  cos_theta = dijkj / pijkj
  theta = dacos(cos_theta)
  delta_theta = theta - bend_teq
  e_bend = e_bend + (delta_theta ** 2) * bend_tk

  scalar = -2.0d0 * bend_tk * delta_theta / dsqrt(1.0d0 - cos_theta ** 2)
  f_i = scalar * (rkj / pijkj - (cos_theta / dijij) * rij)
  f_k = scalar * (rij / pijkj - (cos_theta / dkjkj) * rkj)
  tf_bend(1:3, atom_i) = tf_bend(1:3, atom_i) + f_i
  tf_bend(1:3, atom_j) = tf_bend(1:3, atom_j) - f_i - f_k
  tf_bend(1:3, atom_k) = tf_bend(1:3, atom_k) + f_k
enddo
!f_bend = transpose(tf_bend)
call clock_pop

endsubroutine
