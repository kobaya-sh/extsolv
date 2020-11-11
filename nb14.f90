!subroutine nb14 (tcrd, charge, cn1, cn2, asol, bsol, one_scee, one_scnb, ntypes, &
!  ico, iac, e_coulomb14, tf_coulomb14, e_lj14, tf_lj14, nphi, ijklicp_tmp, tf_tot)
subroutine nb14 (tcrd, charge, cn1, cn2, asol, bsol, one_scee, one_scnb, ntypes, &
  ico, iac, e_coulomb14, e_lj14, nphi, ijklicp_tmp, tf_tot)
implicit none
include 'const.inc'
include 'size.inc'
include 'gau.inc'
include 'mmpara.inc'
real*8              :: CHARGE(m_natom)
real*8              :: CN1(m_ntype*(m_ntype+1)/2)
real*8              :: CN2(m_ntype*(m_ntype+1)/2)
real*8              :: ASOL(m_nphb),  BSOL(m_nphb)
real*8              :: ONE_SCEE(m_nptra),   ONE_SCNB(m_nptra)
integer             :: ntypes
integer             :: ICO(m_ntype**2)
integer             :: IAC(m_natom)

real*8 ,intent(out) :: e_coulomb14
real*8 ,intent(out) :: e_lj14

integer             :: indlist

real*8               :: tf_coulomb14(3, m_natom)
real*8               :: tf_lj14(3, m_natom)
real*8 , intent(out) :: tf_tot(3, m_natom)
integer,intent(in)  :: ijklicp_tmp(*)
integer,intent(in)  :: nphi
real*8              :: len
real*8              :: lil, lil2, lil3, lil6
integer             :: atom_i,atom_l
real*8              :: force(3)
real*8              :: rli(3)

real*8,external     :: length,dihe

integer             :: nb14_lj6_n
integer             :: nb14_lj6_is(m_nphia+m_nphih)
integer             :: nb14_lj6_ls(m_nphia+m_nphih)
real*8              :: nb14_lj6_sccn1s(m_nphia+m_nphih)
real*8              :: nb14_lj6_sccn2s(m_nphia+m_nphih)
real*8              :: nb14_lj6_scqiqjs(m_nphia+m_nphih)
real*8              :: nb14_lj6_qis(m_nphia+m_nphih)
real*8              :: nb14_lj6_qls(m_nphia+m_nphih)
real*8              :: nb14_lj6_cn1s(m_nphia+m_nphih)
real*8              :: nb14_lj6_cn2s(m_nphia+m_nphih)
real*8              :: nb14_lj6_scnbs(m_nphia+m_nphih)
real*8              :: nb14_lj6_scees(m_nphia+m_nphih)

integer             :: nb14_lj10_n
integer             :: nb14_lj10_is(m_nphia+m_nphih)
integer             :: nb14_lj10_ls(m_nphia+m_nphih)
real*8              :: nb14_lj10_scasols(m_nphia+m_nphih)
real*8              :: nb14_lj10_scbsols(m_nphia+m_nphih)
real*8              :: nb14_lj10_scqiqjs(m_nphia+m_nphih)
real*8              :: nb14_lj10_qis(m_nphia+m_nphih)
real*8              :: nb14_lj10_qls(m_nphia+m_nphih)
real*8              :: nb14_lj10_asols(m_nphia+m_nphih)
real*8              :: nb14_lj10_bsols(m_nphia+m_nphih)
real*8              :: nb14_lj10_scnbs(m_nphia+m_nphih)
real*8              :: nb14_lj10_scees(m_nphia+m_nphih)

integer             :: i!,j,k,l,ixyz
logical, parameter  :: idbg = .false.

real*8 , intent(in) :: tcrd(3, *)

call clock_push('nb14')
e_coulomb14  = 0.0d0
e_lj14       = 0.0d0
tf_coulomb14 = 0.0d0
tf_lj14      = 0.0d0

nb14_lj6_n = 0
nb14_lj10_n = 0

do i=1,nphi
  if ( ijklicp_tmp(5*i-2) >= 0 .and. ijklicp_tmp(5*i-1) >= 0 ) then
    atom_i = ijklicp_tmp(5*i-4)/3 + 1
    atom_l = ijklicp_tmp(5*i-1)/3 + 1
    if ( atom_layer ( atom_i ) .or. atom_layer ( atom_l ) ) then
      if ( idbg ) then
        write(0,'(2a10,8x,2i4,a18)') 'nb14_f:','int.1-4',       &
          atom_i,atom_l,' was skipped.'
      endif
      cycle
    endif
    indlist = ico(ntypes*(iac(atom_i)-1)+iac(atom_l))
    if(indlist>=0)then
      nb14_lj6_n = nb14_lj6_n + 1
      nb14_lj6_is(nb14_lj6_n)    = atom_i
      nb14_lj6_ls(nb14_lj6_n)    = atom_l
      nb14_lj6_scqiqjs(nb14_lj6_n) = &
        charge(atom_i) * charge(atom_l) * one_scee(ijklicp_tmp(5*i))
      nb14_lj6_sccn1s(nb14_lj6_n) = &
        cn1(indlist) * one_scnb(ijklicp_tmp(5*i))
      nb14_lj6_sccn2s(nb14_lj6_n) = &
        cn2(indlist) * one_scnb(ijklicp_tmp(5*i))
    else
      nb14_lj10_n = nb14_lj10_n + 1
      nb14_lj10_is(nb14_lj10_n)   = atom_i
      nb14_lj10_ls(nb14_lj10_n)   = atom_l
      nb14_lj10_qis(nb14_lj6_n)   = charge(atom_i)
      nb14_lj10_qls(nb14_lj6_n)   = charge(atom_l)
      nb14_lj10_scees(nb14_lj6_n) = one_scee(ijklicp_tmp(5*i))
      nb14_lj10_asols(nb14_lj6_n) = 0.0d0
      nb14_lj10_bsols(nb14_lj6_n) = 0.0d0
      nb14_lj10_scnbs(nb14_lj6_n) = one_scnb(ijklicp_tmp(5*i))
      nb14_lj10_scqiqjs(nb14_lj10_n) = &
        charge(atom_i) * charge(atom_l) * one_scee(ijklicp_tmp(5*i))
      nb14_lj10_scasols(nb14_lj10_n) = &
        asol(indlist) * one_scnb(ijklicp_tmp(5*i))
      nb14_lj10_scbsols(nb14_lj10_n) = &
        bsol(indlist) * one_scnb(ijklicp_tmp(5*i))
    endif
  endif
enddo
if (nb14_lj10_n > 0) then
  write(0, *) '10-12 potential was untested.'
endif

e_lj14 = 0.0d0
e_coulomb14 = 0.0d0
tf_lj14 = 0.0d0
tf_coulomb14 = 0.0d0

call nb14_lj6(tcrd, &
  nb14_lj6_n, nb14_lj6_is, nb14_lj6_ls, &
  nb14_lj6_sccn1s, nb14_lj6_sccn2s, nb14_lj6_scqiqjs, &
  e_coulomb14, e_lj14, tf_coulomb14, tf_lj14)
call nb14_lj10(tcrd, &
  nb14_lj10_n, nb14_lj10_is, nb14_lj10_ls, &
  nb14_lj10_scasols, nb14_lj10_scbsols, nb14_lj10_scqiqjs, &
  e_coulomb14, e_lj14, tf_coulomb14, tf_lj14)
tf_tot = tf_tot + tf_coulomb14 + tf_lj14
call clock_pop

return
endsubroutine

subroutine nb14_lj6(tcrd, &
  nb14_lj6_n, nb14_lj6_is, nb14_lj6_ls, &
  nb14_lj6_sccn1s, nb14_lj6_sccn2s, nb14_lj6_scqiqjs, &
  e_coulomb14, e_lj14, tf_coulomb14, tf_lj14)
implicit none
include 'const.inc'
include 'size.inc'
include 'gau.inc'
include 'mmpara.inc'

integer, intent(in) :: nb14_lj6_n
integer, intent(in) :: nb14_lj6_is(m_nphia+m_nphih)
integer, intent(in) :: nb14_lj6_ls(m_nphia+m_nphih)
real*8 , intent(in) :: nb14_lj6_sccn1s(m_nphia+m_nphih)
real*8 , intent(in) :: nb14_lj6_sccn2s(m_nphia+m_nphih)
real*8 , intent(in) :: nb14_lj6_scqiqjs(m_nphia+m_nphih)

real*8 ,intent(inout) :: e_coulomb14
real*8 ,intent(inout) :: e_lj14
real*8 ,intent(inout) :: tf_coulomb14(3,m_natom)
real*8 ,intent(inout) :: tf_lj14(3,m_natom)

real*8                :: len
real*8                :: lil, lil2, lil3, lil6
integer               :: atom_i,atom_l
!real*8                :: pos_i(3),pos_l(3)
real*8                :: force(3)
real*8                :: rli(3)
real*8                :: nb14_scqiqj, nb14_sccn1, nb14_sccn2

integer               :: i
logical, parameter    :: idbg = .false.

real*8 , intent(in)  :: tcrd(3, *)

do i=1, nb14_lj6_n
  atom_i    = nb14_lj6_is(i)
  atom_l    = nb14_lj6_ls(i)
  nb14_scqiqj = nb14_lj6_scqiqjs(i)
  nb14_sccn1  = nb14_lj6_sccn1s(i)
  nb14_sccn2  = nb14_lj6_sccn2s(i)

  !pos_i = crd( atom_i, 1:3 )
  !pos_l = crd( atom_l, 1:3 )
  !rli = pos_i - pos_l
  rli = tcrd(1:3, atom_i) - tcrd(1:3, atom_l)
  if (is_pbc) then
    call pbc_parallelpiped(rli, basis_r, basis_k)
  endif
  lil2 = dot_product(rli, rli)
  lil  = dsqrt(lil2)
  lil3 = lil * lil2
  lil6 = lil2 * lil2 * lil2
  len = lil
  if ( len > cutoff ) cycle
  e_coulomb14 = e_coulomb14 + nb14_scqiqj / lil
  force = nb14_scqiqj / lil3 * rli
  tf_coulomb14(1:3, atom_i) = tf_coulomb14(1:3, atom_i) + force
  tf_coulomb14(1:3, atom_l) = tf_coulomb14(1:3, atom_l) - force
  e_lj14 = e_lj14 + (nb14_sccn1 / lil6 - nb14_sccn2) / lil6
  force = 6 * (2 * nb14_sccn1 / lil6 - nb14_sccn2) / (lil6 * lil2) * rli
  tf_lj14(1:3,atom_i) = tf_lj14(1:3,atom_i) + force
  tf_lj14(1:3,atom_l) = tf_lj14(1:3,atom_l) - force
enddo
endsubroutine

subroutine nb14_lj10(tcrd,  &
  nb14_lj10_n, nb14_lj10_is, nb14_lj10_ls, &
  nb14_lj10_scasols, nb14_lj10_scbsols, nb14_lj10_scqiqjs, &
  e_coulomb14, e_lj14, tf_coulomb14, tf_lj14)
implicit none
include 'const.inc'
include 'size.inc'
include 'gau.inc'
!include 'mmcounter.inc'
include 'mmpara.inc'

integer, intent(in)   :: nb14_lj10_n
integer, intent(in)   :: nb14_lj10_is(m_nphia+m_nphih)
integer, intent(in)   :: nb14_lj10_ls(m_nphia+m_nphih)
real*8 , intent(in)   :: nb14_lj10_scasols(m_nphia+m_nphih)
real*8 , intent(in)   :: nb14_lj10_scbsols(m_nphia+m_nphih)
real*8 , intent(in)   :: nb14_lj10_scqiqjs(m_nphia+m_nphih)

real*8 ,intent(inout) :: e_coulomb14
real*8 ,intent(inout) :: e_lj14
real*8 ,intent(inout) :: tf_coulomb14(3, m_natom)
real*8 ,intent(inout) :: tf_lj14(3, m_natom)

real*8                :: lil, lil2, lil3, lil6
integer               :: atom_i,atom_l
!real*8                :: pos_i(3),pos_l(3)
real*8                :: force(3)
real*8                :: rli(3)
real*8                :: nb14_scqiqj, nb14_scasol, nb14_scbsol
real*8, intent(in)    :: tcrd(3, *)

integer               :: i
logical, parameter    :: idbg = .false. 

do i=1, nb14_lj10_n
  atom_i    = nb14_lj10_is(i)
  atom_l    = nb14_lj10_ls(i)
  nb14_scqiqj = nb14_lj10_scqiqjs(i)
  nb14_scasol = nb14_lj10_scasols(i)
  nb14_scbsol = nb14_lj10_scbsols(i)

  !pos_i = crd( atom_i, 1:3 )
  !pos_l = crd( atom_l, 1:3 )
  !rli = pos_i - pos_l
  rli = tcrd(1:3, atom_i) - tcrd(1:3, atom_l)
  if (is_pbc) then
    call pbc_parallelpiped(rli, basis_r, basis_k)
  endif
  lil2 = dot_product(rli, rli)
  lil  = dsqrt(lil2)
  lil3 = lil * lil2
  lil6 = lil2 * lil2 * lil2
  if ( lil > cutoff ) cycle
  e_coulomb14 = e_coulomb14 + nb14_scqiqj / lil
  force = nb14_scqiqj / lil3 * rli
  tf_coulomb14(1:3,atom_i) = tf_coulomb14(1:3,atom_i) + force
  tf_coulomb14(1:3,atom_l) = tf_coulomb14(1:3,atom_l) - force
  ! Lennard-Jones Force ( Does 'one_scnb' have a bug? )
  e_lj14 = e_lj14 + (nb14_scasol / lil**12 - nb14_scbsol / lil**10)
  force = (12 * nb14_scasol / (lil ** 14) - 10 * nb14_scbsol / (lil ** 12)) * rli
  tf_lj14(1:3,atom_i) = tf_lj14(1:3,atom_i) + force
  tf_lj14(1:3,atom_l) = tf_lj14(1:3,atom_l) - force
enddo
endsubroutine

