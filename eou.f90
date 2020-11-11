subroutine eou ( natom, outfile_name, deriv_req, e_tot, dipole, tf_tot )
implicit none
include 'const.inc'
include 'size.inc'
include 'gau.inc'
!include 'mmcounter.inc'
integer     ,intent(in) :: natom
character*60,intent(in) :: outfile_name
real*8      ,intent(in) :: e_tot
real*8      ,intent(in) :: dipole(3)
!real*8      ,intent(in) :: f_tot(m_natom,3)
real*8      ,intent(in) :: tf_tot(3, m_natom)

integer     ,parameter  :: fh_outfile = 9

!integer       :: eof
!character*120 :: line
integer       :: i,j!,k,l,ixyz
logical       :: idbg 
!character*8   :: fmt
!real*8        :: tmp

open(fh_outfile,file=outfile_name(1:len_trim(outfile_name)))
!
!    1. Energy & Dipole Moment 
!
write(fh_outfile,'(4f20.12)') e_tot,(dipole(j),j=1,3)
!
!    2. Force
!
if ( deriv_req >= 1 ) then
  do i=1,natom
    write(fh_outfile,'(3f20.12)') (-tf_tot(j,i),j=1,3)
  enddo
endif
!
!    3. Polarizability ( not available )
!
!write(fh_outfile,'(3f20.12)') 0.0d0,0.0d0,0.0d0
!write(fh_outfile,'(3f20.12)') 0.0d0,0.0d0,0.0d0
!
!    4. Dipole Derivatives ( not available )
!
!do i=1,9*natom
!  write(fh_outfile,'(3f20.12)') 0.0d0,0.0d0,0.0d0
!enddo
!
close(fh_outfile)

endsubroutine
