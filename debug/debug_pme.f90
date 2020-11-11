program debug_pme
implicit none
include 'size.inc'
include 'gau.inc'
include 'mmcounter.inc'
include 'mmpara.inc'
integer           :: natom
real*8            :: charge(m_natom)
real*8            :: e_pme
real*8            :: e_ewald
real*8            :: e_cds
real*8            :: f_pme(m_natom,3) 
real*8            :: f_ewald(m_natom,3) 
real*8            :: f_cds(m_natom,3) 
real*8            :: len
real*8            :: ene
real*8            :: force(m_natom,3)
integer           :: i_k,j_k,k_k
real*8            :: pos_i(3), pos_j(3)
integer           :: NUMEX(m_natom)     ! numex(i) = number of excluded atoms of larger index than i with i
integer           :: INB(m_nnb)         ! inb(i) : list of excluded atoms
integer           :: nnb
integer,parameter :: cell_max=20
real*8,external   :: length
intrinsic dble
integer,parameter :: fh = 0

is_pbc = .true.
is_ewald = .true.
if ( ( fh /= 6 ) .and. ( fh /= 0 ) ) open(fh,file='debug_pme.log')
cell=(/20.0d0,20.0d0,20.0d0/)
is_pbc=.true.
is_ewald=.true.
!write(fh,*) 'start reading crd'
read(*,*) natom
do i=1,natom
  read(*,*) (crd(i,ixyz),ixyz=1,3),charge(i)
enddo
!write(fh,*) 'finish reading crd'
read(*,*)
!read(*,*) numex(1:natom)
nnb=0
do i=1,natom
  nnb=nnb+numex(i)
enddo
!write(fh,*) 'finish reading numex'
read(*,*)
!read(*,*) inb(1:nnb)
!write(fh,*) 'finish reading inb'
!write(fh,*) 'call pme'
call flush(fh)
call pme_e ( charge, natom, e_pme, numex, inb )
write(fh,'(a,f20.12)') ' Energy of PME   Sum  = ', e_pme
!call ewald_e ( charge, natom, e_ewald, numex, inb )
!write(fh,'(a,f20.12)') ' Energy of Ewald Sum  = ', e_ewald
!call cds_e ( charge, natom, e_cds, numex, inb )
!write(fh,'(a,f20.12)') ' Energy of Direct Sum = ', e_cds
!ene=0.0d0
!do i=1,natom
!  pos_i=crd(i,1:3)
!  do j=i+1,natom
!    pos_j=crd(j,1:3)
!    call pbc ( pos_i, pos_j )
!    ene=ene+charge(i)*charge(j)/length(pos_i,pos_j)
!  enddo
!enddo
!write(fh,'(a,f20.12)') ' Energy of Min. Image = ', ene
!
!   Force
!
call pme_f( charge, natom, f_pme, numex, inb )
write(0,'(a)') ' Force of PME'
do i=1,natom
  write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (f_pme(i,ixyz),ixyz=1,3)
enddo
!call ewald_f( charge, natom, f_ewald, numex, inb )
write(0,'(a)') ' Force of Ewald'
do i=1,natom
  write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (f_ewald(i,ixyz),ixyz=1,3)
enddo
f_cds = 0.0d0
!call cds_f( charge, natom, f_cds, numex, inb )
write(0,'(a)') ' Force of Direct Sum'
do i=1,natom
  write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (f_cds(i,ixyz),ixyz=1,3)
enddo
!force = 0.0d0
!do i=1,natom
!  pos_i=crd(i,1:3)
!  do j=i+1,natom
!    pos_j=crd(j,1:3)
!    call pbc ( pos_i, pos_j )
!    force(i,:)=force(i,:)+charge(i)*charge(j)/(length(pos_i,pos_j)**3)*(pos_i-pos_j)
!    force(j,:)=force(j,:)+charge(i)*charge(j)/(length(pos_i,pos_j)**3)*(pos_j-pos_i)
!  enddo
!enddo
!write(0,'(a)') ' Force of Min. Image'
!do i=1,natom
!  write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (force(i,ixyz),ixyz=1,3)
!enddo

if ( ( fh /= 6 ) .and. ( fh /= 0 ) ) close(fh)
endprogram
