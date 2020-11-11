program mkprm
implicit none
include 'size.inc'
include 'prmtop.inc'
character*60 :: prmfile_name

!
!   This utility converts (real).prmtop to (model).prmtop
!
!   Usage: ./mkprm.x (real).prmtop > (model).prmtop
!          optgeom.qmmm.dat is read automatically.
!

call getarg(1,prmfile_name)
call prmtop_r(prmfile_name)
call modprm
call prmtop_w(prmfile_name)

stop
endprogram
