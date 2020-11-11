subroutine nonbon (tcrd, charge, cn1, cn2, asol, bsol, natom, ntypes, &
  numex, ico, iac, inb, isymbl, e_coulomb, e_lj, tf_tot)
implicit none
include 'const.inc'
include 'size.inc'
include 'gau.inc'
!include 'mmcounter.inc'
include 'mmpara.inc'
real*8       :: CHARGE(m_natom)
real*8       :: CN1(m_ntype*(m_ntype+1)/2)
real*8       :: CN2(m_ntype*(m_ntype+1)/2)
real*8       :: ASOL(m_nphb),  BSOL(m_nphb)
integer      :: natom
integer      :: ntypes
integer      :: NUMEX(m_natom)
integer      :: NUMEX_mod(m_natom)
integer      :: NUMEX_qm(m_natom)
integer      :: s_NUMEX_mod(m_natom + 1) ! s_NUMEX(i) = sum_k=1^(i-1) NUMEX(i); expecially, s_NUMEX(1) = 0.
integer      :: s_NUMEX_qm(m_natom + 1) ! s_NUMEX(i) = sum_k=1^(i-1) NUMEX(i); expecially, s_NUMEX(1) = 0.
integer      :: ICO(m_ntype**2)
integer      :: IAC(m_natom)
integer      :: INB(m_nnb)
integer      :: INB_mod(m_nnb)
integer      :: INB_qm(m_nnb)
character*4  :: ISYMBL(m_natom) ! NO LONGER USED but in arguments.
real*8,intent(out) :: e_coulomb
real*8,intent(out) :: e_lj
real*8             :: tf_coulomb(3, m_natom)
real*8             :: tf_lj(3, m_natom)

real*8             :: e_coulomb_ex
real*8             :: e_lj_ex
real*8             :: tf_coulomb_ex(3, m_natom)
real*8             :: tf_lj_ex(3, m_natom)

real*8             :: e_coulomb_qmmm
real*8             :: e_lj_qmmm
real*8             :: tf_coulomb_qmmm(3, m_natom)
real*8             :: tf_lj_qmmm(3, m_natom)

integer            :: qm_atoms(m_natom + 1) ! "+1" is for centinel.
integer            :: nqmatom, nmmatom
integer            :: iqmatom, jqmatom

real*8             :: tcrd(3, m_natom)
real*8 , intent(inout) :: tf_tot(3, m_natom)
call clock_push('nonbon')

call get_qm_atoms(natom, atom_layer, qm_atoms, nqmatom, nmmatom)
call get_numex_mod(natom, NUMEX, INB, NUMEX_mod, INB_mod)
call get_s_numex_mod(natom, NUMEX_mod, s_NUMEX_mod)
call get_qm_numex_mod(natom, nqmatom, qm_atoms, atom_layer, s_NUMEX_mod, INB_mod, s_NUMEX_qm, INB_qm)

!tcrd = transpose(crd)
call qmqm_interaction(is_pbc, is_ewald, natom, nqmatom, qm_atoms, ntypes, &
  cutoff, tcrd, charge, cn1, cn2, asol, bsol, iac, ico, e_coulomb, tf_coulomb, e_lj, tf_lj, basis_r, basis_k)

call qmqm_ex_interaction(is_pbc, is_ewald, natom, nqmatom, s_numex_qm, inb_qm, qm_atoms, ntypes, &
  cutoff, tcrd, charge, cn1, cn2, asol, bsol, iac, ico, e_coulomb_ex, tf_coulomb_ex, e_lj_ex, tf_lj_ex, basis_r, basis_k)
if (is_ee .and. .not. is_ewald) then
  call qmmm_interaction(is_pbc, &
    cutoff, nqmatom, natom, qm_atoms, tcrd, charge, &
    e_coulomb_qmmm, tf_coulomb_qmmm, basis_r, basis_k)
else
  e_coulomb_qmmm=0.0d0
  tf_coulomb_qmmm=0.0d0
endif

e_coulomb = e_coulomb - e_coulomb_ex + e_coulomb_qmmm
tf_coulomb = tf_coulomb - tf_coulomb_ex + tf_coulomb_qmmm
e_lj = e_lj - e_lj_ex
tf_lj = tf_lj - tf_lj_ex
tf_tot = tf_tot + tf_coulomb + tf_lj
!f_coulomb = transpose(tf_coulomb)
!f_lj = transpose(tf_lj)

call clock_pop
return
endsubroutine

subroutine makeforce_coulomb(is_ewald, i, j, charge, len, rij, e_coulomb, tf_coulomb)
implicit none
include 'size.inc'
logical, intent(in) :: is_ewald
real*8 , intent(in) :: charge(*)
real*8 , intent(in) :: len
real*8 , intent(in) :: rij(3)
integer, intent(in) :: i, j
real*8 :: force(3)
real*8 :: e_coulomb
real*8 :: tf_coulomb(3, m_natom)
!if (is_ewald) return

e_coulomb=e_coulomb+charge(i)*charge(j)/len
force = (-charge(i) * charge(j) / (len ** 3)) * rij
tf_coulomb(1:3, i) = tf_coulomb(1:3, i) + force
tf_coulomb(1:3, j) = tf_coulomb(1:3, j) - force
endsubroutine

subroutine makeforce_lj(i, j, indlist, cn1, cn2, asol, bsol, len, rij, e_lj, tf_lj)
implicit none
include 'size.inc'
integer, intent(in) :: i, j, indlist
real*8 , intent(in) :: len
real*8 , intent(in) :: rij(3)
real*8 , intent(in) :: cn1(*), cn2(*)
real*8 , intent(in) :: asol(*), bsol(*)

real*8 :: e_tmp
real*8 :: force(3)
real*8 :: e_lj
real*8 :: tf_lj(3, m_natom)
if(indlist>=0)then
  e_tmp = cn1(indlist)/len**12 - cn2(indlist)/len**6
  force = ( (-12.0d0 * cn1(indlist) / len ** 14 )       &
          + (  6.0d0 * cn2(indlist) / len **  8 ) ) * rij
else
  e_tmp = asol(-indlist) / len ** 12 - bsol(-indlist) / len ** 10
  force = ( (-12.0d0 * asol(-indlist) / len ** 14 )     &
          + ( 10.0d0 * bsol(-indlist) / len ** 12 ) ) * rij
endif
e_lj = e_lj + e_tmp
tf_lj(1:3,i) = tf_lj(1:3,i) + force
tf_lj(1:3,j) = tf_lj(1:3,j) - force
return
endsubroutine

subroutine get_qm_atoms(natom, atom_layer, qm_atoms, nqmatom, nmmatom)
implicit none
integer, intent(in)  :: natom
logical, intent(in)  :: atom_layer(*)
integer, intent(out) :: qm_atoms(*)
integer, intent(out) :: nqmatom, nmmatom
integer              :: i
qm_atoms(1:natom) = 0
nqmatom = 0
nmmatom = natom
do i = 1, natom
  if (atom_layer(i)) then
    qm_atoms(nmmatom) = i
    nmmatom = nmmatom - 1
  else
    nqmatom = nqmatom + 1
    qm_atoms(nqmatom) = i
  endif
enddo
return
endsubroutine

subroutine get_numex_mod(natom, NUMEX, INB, NUMEX_mod, INB_mod)
implicit none
include 'size.inc'
integer, intent(in)  :: natom
integer, intent(in)  :: NUMEX(m_natom)
integer, intent(in)  :: INB(m_nnb)
integer, intent(out) :: NUMEX_mod(m_natom)
integer, intent(out) :: INB_mod(m_nnb)
integer              :: i_numex, j_numex, n_numex
integer              :: i, j
i_numex = 0
j_numex = 0
n_numex = 0
NUMEX_mod = NUMEX
INB_mod = INB
do i=1,natom
  i_numex = n_numex
  n_numex = n_numex + NUMEX_mod(i)
  j = inb(i_numex + 1)
  if (j == 0) then
    NUMEX_mod(i) = 0
  else 
    INB_mod(j_numex + 1: j_numex + NUMEX_mod(i)) &
      = INB_mod(i_numex + 1: i_numex + NUMEX_mod(i))
    j_numex = j_numex + NUMEX_mod(i)
  endif
enddo
return
endsubroutine

subroutine get_s_numex_mod(natom, NUMEX_mod, s_NUMEX_mod)
implicit none
include 'size.inc'
integer, intent(in)  :: natom
integer, intent(in)  :: NUMEX_mod(*)
integer, intent(out) :: s_NUMEX_mod(*)
integer              :: i
s_NUMEX_mod(1) = 0
do i = 1, natom
  s_NUMEX_mod(i + 1) = s_NUMEX_mod(i) + NUMEX_mod(i)
enddo
return
endsubroutine

subroutine qmmm_interaction(is_pbc, &
  cutoff, nqmatom, natom, qm_atoms, tcrd, charge, &
  e_coulomb_qmmm, tf_coulomb_qmmm, basis_r, basis_k)
implicit none
include 'size.inc'
logical, parameter   :: idbg = .false.
logical, intent(in)  :: is_pbc
real*8 , intent(in)  :: cutoff
integer, intent(in)  :: nqmatom, natom
integer, intent(in)  :: qm_atoms(*)
real*8 , intent(in)  :: tcrd(3, m_natom)
real*8 , intent(in)  :: charge(*)
real*8 , intent(out) :: e_coulomb_qmmm
real*8 , intent(out) :: tf_coulomb_qmmm(3, m_natom)
integer              :: i, j, iqmatom, jqmatom
real*8               :: pos_i(3), pos_j(3), rij(3), force(3)
real*8               :: basis_r(3,3), basis_k(3,3)
real*8               :: len
real*8 , external    :: length_vec
e_coulomb_qmmm=0.0d0
tf_coulomb_qmmm=0.0d0
do iqmatom = natom, nqmatom + 1, -1   ! it works as "immatom"
  do jqmatom = 1, nqmatom
    i = qm_atoms(iqmatom)             ! i is mm-atom
    j = qm_atoms(jqmatom)
    pos_i = tcrd(1:3, i)
    pos_j = tcrd(1:3, j)
    rij = pos_j - pos_i
    if (is_pbc) then
      call pbc_parallelpiped(rij, basis_r, basis_k)
    endif
    len = length_vec(rij)
    if (len > cutoff) cycle
    e_coulomb_qmmm=e_coulomb_qmmm+charge(i)*charge(j)/len
    force = (-charge(i) * charge(j) / (len ** 3)) * rij
    tf_coulomb_qmmm(1:3, i) = tf_coulomb_qmmm(1:3,i) + force
    tf_coulomb_qmmm(1:3, j) = tf_coulomb_qmmm(1:3,j) - force
  enddo
enddo
return
endsubroutine

subroutine qmqm_interaction(is_pbc, is_ewald, natom, nqmatom, qm_atoms, ntypes, &
  cutoff, tcrd, charge, cn1, cn2, asol, bsol, iac, ico, e_coulomb, tf_coulomb, e_lj, tf_lj, basis_r, basis_k)
implicit none
include 'size.inc'
logical, parameter   :: idbg = .false.
logical, intent(in)  :: is_pbc, is_ewald
integer, intent(in)  :: natom, nqmatom, ntypes
integer, intent(in)  :: qm_atoms(*)
real*8 , intent(in)  :: cutoff
real*8 , intent(in)  :: tcrd(3, m_natom), charge(*), cn1(*), cn2(*), asol(*), bsol(*)
integer, intent(in)  :: iac(*), ico(*)
real*8 , intent(out) :: e_coulomb, tf_coulomb(3, m_natom), e_lj, tf_lj(3, m_natom)
integer              :: iqmatom, jqmatom, i, j, indlist
real*8               :: pos_i(3), pos_j(3), rij(3)
real*8               :: len
real*8 , external    :: length_vec
real*8               :: basis_r(3,3), basis_k(3,3)
e_coulomb=0.0d0
tf_coulomb=0.0d0
e_lj=0.0d0
tf_lj=0.0d0
do iqmatom = 1, nqmatom
  do jqmatom = iqmatom + 1, nqmatom
    i = qm_atoms(iqmatom)
    j = qm_atoms(jqmatom)
    indlist = ico(ntypes*(iac(i)-1)+iac(j))
    pos_i = tcrd(1:3, i)
    pos_j = tcrd(1:3, j)
    rij = pos_j - pos_i
    if (is_pbc) then
      call pbc_parallelpiped(rij, basis_r, basis_k)
    endif
    len = length_vec(rij)
    if (len > cutoff) cycle
    if (.not. is_ewald) then
      call makeforce_coulomb(is_ewald, i, j, charge, len, rij, e_coulomb, tf_coulomb)
    endif
    call makeforce_lj(i, j, indlist, cn1, cn2, asol, bsol, len, rij, e_lj, tf_lj)
  enddo
enddo
return
endsubroutine

subroutine qmqm_ex_interaction(is_pbc, is_ewald, natom, nqmatom, s_numex_qm, inb_qm, qm_atoms, ntypes, &
  cutoff, tcrd, charge, cn1, cn2, asol, bsol, iac, ico, e_coulomb_ex, tf_coulomb_ex, e_lj_ex, tf_lj_ex, basis_r, basis_k)
implicit none
include 'size.inc'
logical, parameter   :: idbg = .false.
logical, intent(in)  :: is_pbc, is_ewald
integer, intent(in)  :: natom, nqmatom, ntypes
integer, intent(in)  :: qm_atoms(*)
real*8 , intent(in)  :: cutoff
real*8 , intent(in)  :: tcrd(3, m_natom), charge(*), cn1(*), cn2(*), asol(*), bsol(*)
integer, intent(in)  :: iac(*), ico(*)
real*8 , intent(out) :: e_coulomb_ex, tf_coulomb_ex(3, m_natom), e_lj_ex, tf_lj_ex(3, m_natom)
integer              :: iqmatom, jqmatom, i, j, indlist, i_numex
real*8               :: pos_i(3), pos_j(3), rij(3)
real*8               :: len
real*8               :: basis_r(3,3), basis_k(3,3)

integer              :: s_NUMEX_qm(m_natom + 1) ! s_NUMEX(i) = sum_k=1^(i-1) NUMEX(i); expecially, s_NUMEX(1) = 0.
integer              :: INB_qm(m_nnb)

real*8 , external    :: length_vec
e_coulomb_ex=0.0d0
tf_coulomb_ex=0.0d0
e_lj_ex=0.0d0
tf_lj_ex=0.0d0
do iqmatom = 1, nqmatom
  do i_numex = s_NUMEX_qm(iqmatom) + 1, s_NUMEX_qm(iqmatom + 1)
    i = qm_atoms(iqmatom)
    j = inb_qm(i_numex)
    indlist = ico(ntypes*(iac(i)-1)+iac(j))
    pos_i = tcrd(1:3, i)
    pos_j = tcrd(1:3, j)
    rij = pos_j - pos_i
    if (is_pbc) then
      call pbc_parallelpiped(rij, basis_r, basis_k)
    endif
    len = length_vec(rij)
    if (len > cutoff) cycle
    !if (.not. is_ewald) then
      call makeforce_coulomb(is_ewald, i, j, charge, len, rij, e_coulomb_ex, tf_coulomb_ex)
    !endif
    call makeforce_lj(i, j, indlist, cn1, cn2, asol, bsol, len, rij, e_lj_ex, tf_lj_ex)
  enddo
enddo
return
endsubroutine

subroutine get_qm_numex_mod(natom, nqmatom, qm_atoms, atom_layer, s_NUMEX_mod, INB_mod, s_NUMEX_qm, INB_qm)
implicit none
include 'size.inc'
integer, intent(in)  :: natom, nqmatom
integer, intent(in)  :: qm_atoms(*)
logical, intent(in)  :: atom_layer(*)
integer, intent(in)  :: s_NUMEX_mod(*)
integer, intent(in)  :: INB_mod(*)
integer, intent(out) :: s_NUMEX_qm(*)
integer, intent(out) :: INB_qm(*)
integer              :: i_numex
integer              :: i, j, iqmatom, immatom
s_NUMEX_qm(1) = 0
do iqmatom = 1, nqmatom
  i = qm_atoms(iqmatom)
  s_NUMEX_qm(iqmatom + 1) = s_NUMEX_qm(iqmatom)
  do i_numex = s_NUMEX_mod(i) + 1, s_NUMEX_mod(i + 1)
    j = INB_mod(i_numex)
    if (j == 0) exit
    if (atom_layer(j)) cycle
    !indlist = ico(ntypes*(iac(i)-1)+iac(j))
    s_NUMEX_qm(iqmatom + 1) = s_NUMEX_qm(iqmatom + 1) + 1
    INB_qm(s_NUMEX_qm(iqmatom + 1)) = j
  enddo
enddo
return
endsubroutine
