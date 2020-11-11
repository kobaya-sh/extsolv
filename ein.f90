subroutine ein(tcrd, natom_gau, infile_name,deriv_req)
implicit none
include 'const.inc'
!include 'mmcounter.inc'
include 'size.inc'
include 'gau.inc'

!integer       :: eof
character*120 :: line
integer       :: i,j!,k,l,ixyz
logical       :: idbg 
!character*8   :: fmt
!real*8        :: tmp
real*8 , intent(out) :: tcrd(3, m_natom)
integer, intent(out) :: natom_gau

! files
integer,parameter       :: fh_infile = 8
character*60,intent(in) :: infile_name
idbg=.false.
!idbg=.true.
atom_layer=.false.
if(idbg)write(*, *) infile_name(1:len_trim(infile_name))
open(fh_infile,file=infile_name(1:len_trim(infile_name)))
read(fh_infile,'(4i10)') natom_gau,deriv_req,int_charge,int_spin
if(idbg)write(*,*) 'natom_gau=', natom_gau
do i=1,natom_gau
  read(fh_infile,'(a120)') line
  if(idbg)write(*,'(a)')line
  !read(line,'(i10,4f20.12)')atomic_no(i),(crd(i,j),j=1,3),charge_gau(i)
  read(line,'(i10,4f20.12)')atomic_no(i),(tcrd(j, i),j=1,3),charge_gau(i)
  if (index(line,'#6')>0) atom_layer(i) = .true. ! MM atom
enddo
if(idbg)then
  write(0,'(4i10)') natom_gau,deriv_req,int_charge,int_spin
  do i=1,natom_gau
    !write(0,'(i10,4f20.12,l8)')atomic_no(i),(crd(i,j),j=1,3),charge_gau(i),atom_layer(i)
    write(0,'(i10,4f20.12,l8)')atomic_no(i),(tcrd(j, i),j=1,3),charge_gau(i),atom_layer(i)
  enddo
endif
close(fh_infile)

endsubroutine
