program debug_pbc
implicit none
include 'mmpara.inc'
real*8  :: ref(3), target(3)
integer :: i
integer :: eof

cell(1:3) = (/4.0d0,4.0d0,4.0d0/)
do
  write(*,*) 'Cell Size: '
  write(*,'(3f8.4)') cell
  write(*,*) 'Input ref.'
  read(*,*,iostat=eof) (ref(i),i=1,3)
  write(*,*) 'Input target.'
  read(*,*,iostat=eof) (target(i),i=1,3)
  read(*,*,iostat=eof)
  if ( eof .ne. 0 ) exit
  call pbc ( ref, target )
enddo


endprogram
