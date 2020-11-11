subroutine tors ( tcrd, pk, pn, phase, e_tors, tf_tors, nphi, ijklicp_tmp )
implicit none
include 'const.inc'
include 'size.inc'
include 'gau.inc'
include 'mmpara.inc'
real*8              :: PK(m_nptra),    PN(m_nptra),    PHASE(m_nptra) 
real*8 ,intent(out) :: e_tors
!real*8 ,intent(out) :: tf_tors(3, *)
real*8 ,intent(out) :: tf_tors(3, m_natom)
integer,intent(in)  :: ijklicp_tmp(*)
integer,intent(in)  :: nphi

integer             :: atom_i,atom_j,atom_k,atom_l
integer             :: indlist
real*8 , intent(in) :: tcrd(3, *)

real*8              :: rij(3),rkj(3),rkl(3)
real*8              :: n1(3),n2(3)
real*8              :: fbrkj(3),fbrij(3), fcrkl(3),fcrkj(3)
real*8              :: fa, sigma, phi
real*8              :: l1, l2, dijkj, dkjkj, dklkj
real*8              :: work

integer             :: tors_n
integer             :: tors_is(m_nphia+m_nphih)
integer             :: tors_js(m_nphia+m_nphih)
integer             :: tors_ks(m_nphia+m_nphih)
integer             :: tors_ls(m_nphia+m_nphih)
real*8              :: tors_pns(m_nphia+m_nphih)
real*8              :: tors_pks(m_nphia+m_nphih)
real*8              :: tors_phases(m_nphia+m_nphih)
real*8              :: tors_pn, tors_pk, tors_phase

integer             :: i

logical, parameter  :: idbg =.false.

intrinsic dsin,dcos,iabs,dot_product

call clock_push('torsion')

tors_n = 0
if (idbg) write(0,*) 'nphi = ',nphi
do i=1,nphi
  !
  !    1. Set General Variables
  !
  atom_i = ijklicp_tmp(5*i-4)/3+1
  atom_j = ijklicp_tmp(5*i-3)/3+1
  atom_k = iabs(ijklicp_tmp(5*i-2))/3+1
  atom_l = iabs(ijklicp_tmp(5*i-1))/3+1
  if ( atom_layer ( atom_i ) .or. atom_layer ( atom_j ) .or.           &
       atom_layer ( atom_k ) .or. atom_layer ( atom_l ) ) then
    if ( idbg ) then
      write(0,'(2a10,4i4,a18)') 'Tors_e:','Dihedral',                  &
        atom_i,atom_j,atom_k,atom_l,' was skipped.'
    endif
    cycle
  endif
  indlist = ijklicp_tmp(5*i)

  tors_n = tors_n + 1
  tors_is(tors_n) = ijklicp_tmp(5*i-4)/3+1
  tors_js(tors_n) = ijklicp_tmp(5*i-3)/3+1
  tors_ks(tors_n) = iabs(ijklicp_tmp(5*i-2))/3+1
  tors_ls(tors_n) = iabs(ijklicp_tmp(5*i-1))/3+1
  tors_pns(tors_n) = pn(indlist) 
  tors_pks(tors_n) = pk(indlist) 
  tors_phases(tors_n) = phase(indlist) 
enddo

call clock_look('tors')
e_tors  = 0.0d0
!tf_tors = 0.0d0
do i = 1, tors_n
  atom_i      = tors_is(i)
  atom_j      = tors_js(i)
  atom_k      = tors_ks(i)
  atom_l      = tors_ls(i)
  tors_pn     = tors_pns(i) 
  tors_pk     = tors_pks(i)
  tors_phase  = tors_phases(i)

  rij = tcrd(1:3, atom_j) - tcrd(1:3, atom_i)
  rkj = tcrd(1:3, atom_j) - tcrd(1:3, atom_k)
  rkl = tcrd(1:3, atom_l) - tcrd(1:3, atom_k)
  if (is_pbc) then
    call pbc_parallelpiped(rij, basis_r, basis_k)
    call pbc_parallelpiped(rkj, basis_r, basis_k)
    call pbc_parallelpiped(rkl, basis_r, basis_k)
  endif
  call cross(rij, rkj, n1)
  call cross(rkj, rkl, n2)
  l1 = dot_product(n1, n1)
  l2 = dot_product(n2, n2)
  dijkj = dot_product(rij, rkj)
  dkjkj = dot_product(rkj, rkj)
  dklkj = dot_product(rkl, rkj)
  phi = dacos(dot_product(n1, n2) / dsqrt(l1 * l2))
  sigma = dsign(1.0d0, dot_product(n1, rkl)) ! sign(rij . rkj x rkl)

  work = phi * tors_pn - tors_phase           ! phi is no longer "phi"
  e_tors = e_tors + (dcos(work) + 1.0d0) * tors_pk 
  fa = -tors_pk * tors_pn * dsin(work) * sigma / dsqrt(dkjkj)
  work = fa / l1
  fbrij = (work * dijkj) * n1
  fbrkj = (work * dkjkj) * n1
  work = -fa / l2
  fcrkj = (work * dkjkj) * n2
  fcrkl = (work * dklkj) * n2

  tf_tors(1:3, atom_i) = tf_tors(1:3, atom_i) + fbrkj
  tf_tors(1:3, atom_j) = tf_tors(1:3, atom_j) + (fbrij - fbrkj - fcrkl) 
  tf_tors(1:3, atom_k) = tf_tors(1:3, atom_k) + (fcrkl - fcrkj - fbrij) 
  tf_tors(1:3, atom_l) = tf_tors(1:3, atom_l) + fcrkj
enddo

call clock_pop
return
endsubroutine
