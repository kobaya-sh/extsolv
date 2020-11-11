subroutine bond ( tcrd, rk, req, e_bond, tf_bond, nbon, ijicb_tmp )
implicit none
include 'const.inc'
include 'size.inc'
include 'gau.inc'
include 'mmpara.inc'
real*8 , intent(out) :: e_bond
real*8 , intent(out) :: tf_bond(3, m_natom)
integer, intent(in)  :: nbon
integer, intent(in)  :: ijicb_tmp(*)

integer              :: atom_i, atom_j
integer              :: indlist
real*8 , intent(in)  :: tcrd(3, *)

real*8               :: rk(m_numbnd), req(m_numbnd)

real*8               :: rij(3)
real*8               :: lij, delta_lij
real*8               :: f_i(3)              ! Force added to atom_i

integer              :: bond_n
integer              :: bond_is(m_nbona+m_nbonh)
integer              :: bond_js(m_nbona+m_nbonh)
real*8               :: bond_reqs(m_nbona+m_nbonh)
real*8               :: bond_rks(m_nbona+m_nbonh)
real*8               :: bond_req, bond_rk

integer              :: i, k
logical, parameter   :: idbg = .false.

call clock_push('bond')
if (idbg) write(0,*) 'nbon = ',nbon

bond_n = 0
do i=1,nbon
  !
  !   1. Set General Variable
  !
  atom_i  = ijicb_tmp(3*i-2)/3+1
  atom_j  = ijicb_tmp(3*i-1)/3+1
  if ( atom_layer ( atom_i ) .or. atom_layer ( atom_j ) ) then
    if ( idbg ) write(0,'(2a10,8x,2i4,a18)')          &
      'Bond_e:','Bond',atom_i,atom_j,' was skipped.'
    cycle
  endif
  indlist = ijicb_tmp(3*i)
  bond_n = bond_n + 1
  bond_is(bond_n)   = ijicb_tmp(3*i-2)/3+1
  bond_js(bond_n)   = ijicb_tmp(3*i-1)/3+1
  bond_reqs(bond_n) = req(indlist)
  bond_rks(bond_n)  = rk(indlist)
enddo

e_bond = 0.0d0
!tf_bond = 0.0d0
do i = 1, bond_n
  atom_i = bond_is(i)
  atom_j = bond_js(i)
  bond_req = bond_reqs(i)
  bond_rk  = bond_rks(i)
  rij = tcrd(1:3, atom_j) - tcrd(1:3, atom_i)
  if ( is_pbc ) then
    call pbc_parallelpiped(rij, basis_r, basis_k)
  endif
  lij = dsqrt(dot_product(rij, rij))
  delta_lij = lij - bond_req
  e_bond = e_bond + (delta_lij ** 2) * bond_rk
  f_i = (2.0d0 * bond_rk * delta_lij / lij) * rij
  tf_bond(1:3, atom_i) = tf_bond(1:3, atom_i) + f_i
  tf_bond(1:3, atom_j) = tf_bond(1:3, atom_j) - f_i
enddo
call clock_pop

return
endsubroutine
