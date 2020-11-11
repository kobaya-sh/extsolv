program debug_ewald
implicit none
include 'size.inc'
include 'gau.inc'
include 'mmcounter.inc'
include 'mmpara.inc'
integer           :: natom
real*8            :: charge(m_natom)
real*8            :: e_ewald
real*8            :: f_ewald(m_natom,3) 
real*8            :: len
real*8            :: ene
real*8            :: force(m_natom,3)
integer           :: i_k,j_k,k_k
real*8            :: pos_i(3), pos_j(3)
integer           :: NUMEX(m_natom)     ! numex(i) = number of excluded atoms of larger index than i with i
integer           :: INB(m_nnb)         ! inb(i) : list of excluded atoms
integer           :: nnb
integer,parameter :: cell_max=50
real*8,external   :: length
intrinsic dble
cell=(/20.0d0,20.0d0,20.0d0/)
basis_r=reshape((/20.0d0,0.0d0,0.0d0,0.0d0,20.0d0,0.0d0,0.0d0,0.0d0,20.0d0/),shape(basis_r))
basis_k=reshape((/0.05d0,0.0d0,0.0d0,0.0d0,0.05d0,0.0d0,0.0d0,0.0d0,0.05d0/),shape(basis_k))
is_pbc=.true.
is_ewald=.true.
read(*,*) natom
do i=1,natom
  read(*,*) (crd(i,ixyz),ixyz=1,3),charge(i)
enddo
read(*,*)
read(*,*) numex(1:natom)
nnb=0
do i=1,natom
  nnb=nnb+numex(i)
enddo
read(*,*)
read(*,*) inb(1:nnb)
e_ewald=1.0d0
call ewald_e ( charge, natom, e_ewald, numex, inb )
write(0,'(a,f20.12)') ' Energy of Ewald Sum  = ', e_ewald

call pme ( charge, natom, e_ewald, f_ewald, numex, inb )
write(0,'(a,f20.12)') ' Energy of PME        = ', e_ewald

stop
ene=0.0d0
do i=1,natom
  do j=1,natom
    do i_k=-cell_max,cell_max
      do j_k=-cell_max,cell_max
        do k_k=-cell_max,cell_max
        !do k_k=0,1
          pos_i=crd(i,1:3)
          pos_j(1)=crd(j,1)+dble(i_k)*cell(1)
          pos_j(2)=crd(j,2)+dble(j_k)*cell(2)
          pos_j(3)=crd(j,3)+dble(k_k)*cell(3)
          !pos_j=crd(j,1:3)
          !pos_j=pos_j+dble(i_k)*basis_r(1:3,1)
          !pos_j=pos_j+dble(j_k)*basis_r(1:3,2)
          !pos_j=pos_j+dble(k_k)*basis_r(1:3,3)
          len=length(pos_i,pos_j)
          !write(*,'(3i4,2f12.8)') i_k,j_k,k_k,ene,len
          !write(0,*)charge(i),charge(j),len,charge(i)*charge(j)/len
          if ( i_k == 0 .and. j_k == 0 .and. k_k == 0 ) then
            if (( len > 1.0d-6 ) .or. (i /= j))then
              ene=ene+0.5d0*charge(i)*charge(j)/len
            endif
          else
            !ene=ene+charge(i)*charge(j)/len
            ene=ene+0.5d0*charge(i)*charge(j)/len
          endif
        enddo
      enddo
    enddo
  enddo
enddo
write(0,'(a,f20.12)') ' Energy of Direct Sum = ', ene
ene=0.0d0
do i=1,natom
  pos_i=crd(i,1:3)
  do j=i+1,natom
    pos_j=crd(j,1:3)
    call pbc ( pos_i, pos_j )
    if ( length(pos_i,pos_j) <= cutoff ) then
      ene=ene+charge(i)*charge(j)/length(pos_i,pos_j)
    endif
  enddo
enddo
write(0,'(a,f20.12)') ' Energy of Min. Image = ', ene
stop
!
!   Force
!
write(0,'(a)') ' Force of Ewald Sum '
do i=1,natom
  write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (f_ewald(i,ixyz),ixyz=1,3)
enddo
force=0.0d0
do i=1,natom
  do j=1,natom
    do i_k=-cell_max,cell_max
      do j_k=-cell_max,cell_max
        do k_k=-cell_max,cell_max
          pos_i=crd(i,1:3)
          pos_j(1)=crd(j,1)+dble(i_k)*cell(1)
          pos_j(2)=crd(j,2)+dble(j_k)*cell(2)
          pos_j(3)=crd(j,3)+dble(k_k)*cell(3)
          len=length(pos_i,pos_j)
          if ( len > 1.0d-3 ) then
            force(i,1:3)=force(i,1:3)+charge(i)*charge(j)/(len**3)*(pos_i-pos_j)
          endif
        enddo
      enddo
    enddo
  enddo
enddo
write(0,'(a)') ' Force of Direct sum '
do i=1,natom
  write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (force(i,ixyz),ixyz=1,3)
enddo
force = 0.0d0
do i=1,natom
  pos_i=crd(i,1:3)
  do j=i+1,natom
    pos_j=crd(j,1:3)
    call pbc ( pos_i, pos_j )
    force(i,:)=force(i,:)+charge(i)*charge(j)/(length(pos_i,pos_j)**3)*(pos_i-pos_j)
    force(j,:)=force(j,:)+charge(i)*charge(j)/(length(pos_i,pos_j)**3)*(pos_j-pos_i)
  enddo
enddo
write(0,'(a)') ' Force of Min. Image '
do i=1,natom
  write(0,'(a,i4,a,3f20.12)') 'i =', i, ':', (force(i,ixyz),ixyz=1,3)
enddo

stop
endprogram
