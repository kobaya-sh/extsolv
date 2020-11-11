program debug_theta
integer p,largek,k,kpk
real*8 x
intrinsic dble
write(*,*) 'p,largek,x,k'
read(*,*)  p,largek,x,k
write(*,*) p,largek,x,k
write(*,*) 'kpk=[Kx]-p+1,x-dble(kpk),k-kpk'
  kpk   = nint ( x * dble ( largek ) - 0.5d0 ) - p + 1
write(*,*) kpk,x-dble(kpk),k-kpk
write(*,*) theta ( p,largek,x,k )
endprogram

real*8  function theta ( p, largek, x, k )
implicit none
integer             :: p, largek, k
real*8              :: x
integer             :: kxint                      ! kxint = [Kx]
integer             :: kpk                        ! k_(p,K)(x) = [Kx] - p + 1
intrinsic dabs, nint, dble, int
real*8, external    :: phi
logical,parameter   :: idbg = .true.
if(idbg) write(*,*) p,largek,x,k
kxint = nint ( x * dble ( largek ) - 0.5d0 )
!kxint = int ( x * dble ( largek ) )
! if ( x == l/K ) <=> ( int(Kx) == K*int(x) ) then x encounters singular point of phi.
if(idbg) write(*,*) kxint
if ( kxint == k ) then 
  if(idbg) write(*,*)'case1'
  theta = 1.0d0
elseif ( dabs ( dble ( kxint ) - dble ( largek ) * x ) < 1.0d-14 ) then
  if(idbg) write(*,*)'case0'
  theta = 0.0d0
else
  if(idbg) write(*,*)'casecalc'
  kpk   = nint ( x * dble ( largek ) - 0.5d0 ) - p + 1
  if(idbg) write(*,*) kpk
  theta = phi ( p, largek, ( x - dble ( kpk ) ), ( k - kpk ) )
endif
return
endfunction

real*8  function phi ( p, largek, x, k )
implicit none
integer             :: p, largek, k
real*8              :: x
integer             :: l                          ! counter
real*8              :: denom                      ! denominator
real*8              :: numer                      ! numerator
integer             :: order                      ! order of Lagrangian interpolation: 2p-1
integer, external   :: combination
logical, parameter  :: idbg = .false.
intrinsic dble
phi = 0.0d0
order = 2 * p - 1
if ( ( 0 <= k ) .and. ( k <= order ) ) then
  numer =  dble ( ( ( -1 ) ** k ) * combination ( order, k ) )          &
        / ( x - dble ( k ) / dble ( largek ) )
  denom = 0.0d0
  do l = 0, order
    denom = denom + dble ( ( ( -1 ) ** l ) * combination ( order, l ) ) &
          / ( x - dble ( l ) / dble ( largek ) )
!  if ( idbg ) write(0,*) 'DBG@phi:numer,denom=',numer,denom
  enddo
  phi = numer / denom
  if ( idbg ) write(0,*) 'DBG@phi:x,k,numer,denom=',x,k,numer,denom
endif
return
endfunction

integer function combination ( n, r )
!
!   nCr for n <= 50
!   Overflow occurs when n > 50.
!
implicit none
integer             :: n,r
integer             :: i
if ( n > 50 ) stop 'Overflow may occur in funcion "combination@pme_e.f90".'
combination = 1
do i = 1, r
  combination = combination * ( n-i+1 ) / i
enddo
return
endfunction
