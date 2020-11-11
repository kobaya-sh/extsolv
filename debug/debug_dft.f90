program main
implicit none
integer    , parameter     :: n = 4
!complex*16                 :: x(0:n-1)
complex*16                 :: x(n)
!complex*16                 :: f(0:n-1)
complex*16                 :: f(n)
integer                    :: i
intrinsic dble
!do i = 0, n-1
do i = 1, n
  x(i) = dble(i)
enddo
!do i = 0, n-1
do i = 1, n
  write(*,'(2f12.8)') x(i)
enddo
call dft ( n, x, -1 )
!do i = 0, n-1
do i = 1, n
  write(*,'(2f12.8)') x(i)
enddo
call dft ( n, x,  1 )
!do i = 0, n-1
do i = 1, n
  write(*,'(2f12.8)') x(i)
enddo
endprogram
