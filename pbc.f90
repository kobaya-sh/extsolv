subroutine pbc ( ref, target )
implicit none
include 'mmpara.inc'
real*8,intent(in)    :: ref(3)
real*8,intent(inout) :: target(3)
real*8               :: org(3)
logical              :: idbg
logical              :: changed
integer              :: i

intrinsic dnint

idbg = .false.
idbg = .true.

!if ( idbg ) then
!  write(0,'(a,3f20.12)') ' cell              = ', cell
!  write(0,'(a,3f20.12)') ' ref               = ', ref
!  write(0,'(a,3f20.12)') ' target ( before ) = ', target
!endif

org = target
changed = .false.
do i=1,3
  target(i) = target(i) - cell(i) * dnint ( ( target(i) - ref(i) )  / cell(i) )
  changed = changed .or. (org(i) /= target(i))
enddo
!if (idbg) then
!  if (changed) then
!    write(*, *) 'org:', org
!    write(*, *) 'new:', target
!  endif
!endif

!if ( idbg ) then
!  write(0,'(a,3f20.12)') ' target ( after )  = ', target
!endif
!
return
endsubroutine

subroutine pbc_parallelpiped(target, basis_r, basis_k)
implicit none
!include 'mmpara.inc'
!real*8,intent(in)    :: ref(3)
real*8,intent(inout) :: target(3)
real*8               :: org(3)
real*8, intent(in)   :: basis_r(3,3), basis_k(3,3)
logical, parameter   :: idbg = .false.
logical              :: changed
integer              :: i

intrinsic dnint

!if ( idbg ) then
!  write(0,'(a,3f20.12)') ' cell              = ', cell
!  write(0,'(a,3f20.12)') ' ref               = ', ref
!  write(0,'(a,3f20.12)') ' target ( before ) = ', target
!endif

org = target
changed = .false.
target = target - matmul(basis_r, dnint(matmul(basis_k, target)))
do i=1,3
  !target(i) = target(i) - cell(i) * dnint ( ( target(i) - ref(i) )  / cell(i) )
  changed = changed .or. (org(i) /= target(i))
enddo
if (idbg) then
  if (changed) then
    write(*, *) 'org:', org
    write(*, *) 'new:', target
  endif
endif

!if ( idbg ) then
!  write(0,'(a,3f20.12)') ' target ( after )  = ', target
!endif
!
return
endsubroutine
