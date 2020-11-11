subroutine moment (tcrd, charge, natom, tot_charge, center, dipole)
implicit none
include 'const.inc'
include 'size.inc'
include 'gau.inc'
!include 'mmcounter.inc'
real*8 , intent(in) :: tcrd(3, *)
integer,intent(in)  :: natom
real*8 ,intent(in)  :: CHARGE(m_natom)
real*8 ,intent(out) :: tot_charge
real*8 ,intent(out) :: dipole(3)
real*8 ,intent(out) :: center(3)
!integer       :: eof
!character*120 :: line
integer       :: i!,j,k,l,ixyz
!logical       :: idbg 
!character*8   :: fmt
!real*8        :: tmp
intrinsic dble
!
!    1. dipole_moment
!
!    dipole = sum(charge*(r_vec-r0_vec))
!           = sum(charge*r_vec) - sum(charge)*r0_vec 
!
!    where r0_vec is geomtrical gravity point of system
!
tot_charge = 0.0d0
dipole = 0.0d0
center = 0.0d0
do i=1,natom
  tot_charge = tot_charge + charge(i)
!  dipole = dipole + crd(i,1:3) * dble ( charge(i) )
!  center = center + crd(i,1:3)
  dipole = dipole + tcrd(1:3,i) * dble (charge(i))
  center = center + tcrd(1:3,i)
enddo
center = center / natom
tot_charge = tot_charge / scalecharge
!dipole = dipole - tot_charge * center
dipole = dipole / scalecharge

return
endsubroutine
