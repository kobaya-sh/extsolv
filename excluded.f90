subroutine nonbon ( charge, cn1, cn2, asol, bsol, natom, ntypes, &
  numex, ico, iac, inb, isymbl, e_coulomb, f_coulomb, e_lj, f_lj )
implicit none
include 'const.inc'
include 'size.inc'
include 'gau.inc'
include 'mmcounter.inc'
include 'mmpara.inc'
real*8       :: CHARGE(m_natom)
real*8       :: CN1(m_ntype*(m_ntype+1)/2)
real*8       :: CN2(m_ntype*(m_ntype+1)/2)
real*8       :: ASOL(m_nphb),  BSOL(m_nphb)
integer      :: natom
integer      :: ntypes
integer      :: NUMEX(m_natom)
integer      :: ICO(m_ntype**2)
integer      :: IAC(m_natom)
integer      :: INB(m_nnb)
character*4  :: ISYMBL(m_natom)
real*8,intent(out) :: e_coulomb
real*8,intent(out) :: e_lj
real*8,intent(out) :: f_coulomb(m_natom,3)
real*8,intent(out) :: f_lj(m_natom,3)
integer            :: n_numex ! pointer max
integer            :: i_numex ! pointer
real*8             :: force(3)
real*8             :: len
integer            :: indlist
real*8             :: pos_i(3),pos_j(3)
real*8             :: rij(3)

real*8,external    :: length_vec

real*8             :: e_coulomb_ex
real*8             :: e_lj_ex
real*8             :: f_coulomb_ex(m_natom,3)
real*8             :: f_lj_ex(m_natom,3)

open(99, file='nonbon')
call clock_push('nonbon')

e_coulomb=0.0d0
f_coulomb=0.0d0
e_lj=0.0d0
f_lj=0.0d0
! atom_i = i; atom_j = j
do i=1,natom
  do j=i+1,natom
    indlist = ico(ntypes*(iac(i)-1)+iac(j))
    pos_i = crd ( i, 1:3 )
    pos_j = crd ( j, 1:3 )
    if ( is_pbc ) then
      if ( idbg ) write(0,'(a,3f12.6)') 'pos_i',pos_i
      if ( idbg ) write(0,'(a,3f12.6)') 'pos_j',pos_j
      call pbc ( pos_i, pos_j )
      if ( idbg ) write(0,'(a,3f12.6)') 'pos_j',pos_j
    endif
    rij = pos_j - pos_i
    len = length_vec(rij)
    call makeforce(e_coulomb, f_coulomb, e_lj, f_lj)
  enddo
enddo

e_coulomb_ex=0.0d0
f_coulomb_ex=0.0d0
e_lj_ex=0.0d0
f_lj_ex=0.0d0
n_numex=0
i_numex=1
! atom_i = i; atom_j = j
do i =1,natom
  i_numex = n_numex
  n_numex=n_numex+NUMEX(i)
  do i_numex = i_numex + 1, n_numex
    j = inb(i_numex)
    if (j == 0) exit
    indlist = ico(ntypes*(iac(i)-1)+iac(j))
    pos_i = crd (i, 1:3)
    pos_j = crd (j, 1:3)
    if ( is_pbc ) then
      if ( idbg ) write(0,'(a,3f12.6)') 'pos_i',pos_i
      if ( idbg ) write(0,'(a,3f12.6)') 'pos_j',pos_j
      call pbc ( pos_i, pos_j )
      if ( idbg ) write(0,'(a,3f12.6)') 'pos_j',pos_j
    endif
    rij = pos_j - pos_i
    len = length_vec(rij)
    call makeforce(e_coulomb_ex, f_coulomb_ex, e_lj_ex, f_lj_ex)
  enddo
enddo

e_coulomb = e_coulomb - e_coulomb_ex
f_coulomb = f_coulomb - f_coulomb_ex
e_lj = e_lj - e_lj_ex
f_lj = f_lj - f_lj_ex

!do i = 1, natom
!  write(0,'(a,i4,3f12.8)') 'i=',i,(f_coulomb(i,ixyz),ixyz=1,3)
!enddo
call clock_pop
return
    
contains

  subroutine makeforce(e_coulomb, f_coulomb, e_lj, f_lj)
  real*8             :: e_coulomb
  real*8             :: e_lj
  real*8             :: f_coulomb(m_natom,3)
  real*8             :: f_lj(m_natom,3)
  write(99, *) i, j
  ! Coulomb Force
  ! BOTH QM-QM charge and QM-MM charge are counted.
  ! ( Only MM-MM charge is neglected ) 
!write(0,*) 'TEST'
!write(0,*) charge(i), charge(j), len, rij, force
  if ( .not. is_ewald ) then
    !write(0, *) 'atom_layer(i, j), is_ee', i, j, atom_layer(i), atom_layer(j), is_ee
    if ((is_ee .and. (atom_layer(i) .and. atom_layer(j))) &
      .or. ((.not. is_ee) .and. (atom_layer(i) .or. atom_layer(j)))) then
      if ( idbg ) then
        write(0,'(2a10,8x,2i4,a18)') 'nonbon_f:','Coulomb',i,j,' was skipped.'
      endif
    else
      if ( len <= cutoff ) then
        e_coulomb=e_coulomb+charge(i)*charge(j)/len
        force = -1.0d0                                         &
              * ( charge(i) * charge (j) )                     &
              / ( len ** 3 ) * rij
        f_coulomb(i,1:3) = f_coulomb(i,1:3) + force
        f_coulomb(j,1:3) = f_coulomb(j,1:3) - force
      endif
    endif
  endif
  ! Lennard-Jones Force
  ! Only QM-QM VDW are counted.
  ! ( MM-MM VDW and QM-MM VDW are neglected ) 
  if ( atom_layer(i) .or. atom_layer(j) ) then
    if ( idbg ) then
      write(0,'(2a10,8x,2i4,a18)') 'nonbon_f:','VDW',i,j,' was skipped.'
    endif
  else
    if(indlist>=0)then
      if ( len <= cutoff ) then
        e_lj = e_lj                                             &
             + cn1(ico(ntypes*(iac(i)-1)+iac(j)))/len**12       &
             - cn2(ico(ntypes*(iac(i)-1)+iac(j)))/len**6
        force = (                                               &
                  (  -12.0d0 * cn1(indlist) / len ** 14 )       &
                  + (  6.0d0 * cn2(indlist) / len **  8 )       &
                )                                               &
                * rij
        f_lj(i,1:3) = f_lj(i,1:3) + force
        f_lj(j,1:3) = f_lj(j,1:3) - force
      endif
    else
      if ( len <= cutoff ) then
        e_lj = e_lj                                             &
             + asol(-indlist) / len ** 12                       &
             - bsol(-indlist) / len ** 10
        force = (                                               &
                  (  -12.0d0 * asol(-indlist) / len ** 14 )     &
                  + ( 10.0d0 * bsol(-indlist) / len ** 12 )     &
                )                                               &
                * rij
        f_lj(i,1:3) = f_lj(i,1:3) + force
        f_lj(j,1:3) = f_lj(j,1:3) - force
      endif
    endif
  endif
  return
  endsubroutine

endsubroutine
