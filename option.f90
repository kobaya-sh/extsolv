subroutine option ( prmfile_name, logfile_name, optfile_name, log_level, fh_log )
implicit none
include 'mmpara.inc'
real*8            :: cell_x       =  0.0d0
real*8            :: cell_y       =  0.0d0
real*8            :: cell_z       =  0.0d0
real*8            :: alpha        = 90.0d0
real*8            :: beta         = 90.0d0
real*8            :: gamma        = 90.0d0
real*8            :: ca,cb,cg,sg ! cos(alpha) etc.
real*8            :: v
character*60      :: prmfile_name
character*60      :: logfile_name
character*60      :: optfile_name
integer           :: log_level
integer           :: fh_log
integer,parameter :: fh_opt       = 11
real*8 ,parameter :: pi = 3.1415926535897932384d0

logical           :: idbg

intrinsic dsin, dcos, dsqrt

data is_pbc      /.false./
data is_ewald    /.false./
data is_ee       /.true./

namelist /filename/ prmfile_name,logfile_name,log_level
namelist /pbc/      is_pbc,is_ewald,is_ee,&
                    cell_x,cell_y,cell_z, &
                    alpha,beta,gamma

idbg = .true.
!
!   1. Make Option Filename
!
if     ( trim(optfile_name) == 'M' ) then
  optfile_name = 'model.opt'
elseif ( trim(optfile_name) == 'R' ) then
  optfile_name = 'real.opt'
elseif ( trim(optfile_name) == 'T' ) then
  optfile_name = 'test.opt'
endif
!
!   2. Read Option File
!
prmfile_name = 'test.prmtop'
logfile_name = 'mm.log'
open(fh_opt,file=optfile_name)
read(fh_opt,nml=filename)
read(fh_opt,nml=pbc)
close(fh_opt)
cell=(/cell_x,cell_y,cell_z/)

ca=dcos(alpha*pi/180.0d0)
cb=dcos(beta *pi/180.0d0)
cg=dcos(gamma*pi/180.0d0)
sg=dsin(gamma*pi/180.0d0)

v=dsqrt(1.0d0-ca*ca-cb*cb-cg*cg+2.0d0*ca*cb*cg)

basis_r(1,1)=cell_x
basis_r(2,1)=0.0d0
basis_r(3,1)=0.0d0

basis_r(1,2)=cell_y*cg
basis_r(2,2)=cell_y*sg
basis_r(3,2)=0.0d0

basis_r(1,3)=cell_z*cb
basis_r(2,3)=cell_z*(ca-cb*cg)/sg
basis_r(3,3)=cell_z*v/sg

basis_k(1,1)=1.0d0/cell_x
basis_k(2,1)=0.0d0
basis_k(3,1)=0.0d0

basis_k(1,2)=-cg/(cell_x*sg)
basis_k(2,2)=1.0d0/(cell_y*sg)
basis_k(3,2)=0.0d0

basis_k(1,3)=(ca*cg-cb)/(cell_x*v*sg)
basis_k(2,3)=(cb*cg-ca)/(cell_y*v*sg)
basis_k(3,3)=sg/(cell_z*v)

if ( ( trim(logfile_name) == 'STDOUT' ) .or. ( trim(logfile_name) == 'stdout' ) ) then
  fh_log = 6
elseif ( ( trim(logfile_name) == 'STDERR' ) .or. ( trim(logfile_name) == 'stderr' ) ) then
  fh_log = 0
endif
!
!   3. Check Contradiction
!
if ( is_ewald .and. ( .not. is_pbc ) ) then
  stop "'is_ewald = .true.' conflicts to 'is_pbc = .false.'"
endif

if ( is_pbc .and. (           &
     ( cell_x == 0.0d0 ) .or. &
     ( cell_y == 0.0d0 ) .or. &
     ( cell_z == 0.0d0 ) ) ) then

 stop "Cell size input is not completed."
endif

return
endsubroutine
