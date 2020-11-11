subroutine zfft1d(z, n1, dir)
implicit none
integer n1
integer dir
complex*16 z(0:n1-1)
if (dir /= 1 .and. dir /= -1) then
  stop 'zfft1d: "dir" must be +-1.'
endif
call core(n1, z, dir)
if (dir == -1) then
  z = z / dcmplx(n1, 0)
endif
return
endsubroutine

subroutine zfft2d(z, n1, n2, dir)
implicit none
integer n1, n2
integer dir
integer i
complex*16 z(0:n1-1, 0:n2-1)
if (dir /= 1 .and. dir /= -1) then
  stop 'zfft2d: "dir" must be +-1.'
endif
do i = 0, n2 - 1
  call core(n1, z(0:n1-1,i), dir)
enddo
do i = 0, n1 - 1
  call core(n2, z(i,0:n2-1), dir)
enddo
if (dir == -1) then
  z = z / dcmplx(n1 * n2, 0)
endif
return
endsubroutine

subroutine zfft3d(z, n1, n2, n3, dir)
implicit none
integer n1, n2, n3
integer dir
complex*16 z(0:n1-1, 0:n2-1, 0:n3-1)
integer i, j
if (dir /= 1 .and. dir /= -1) then
  stop 'zfft3d: "dir" must be +-1.'
endif
do i = 0, n2 - 1
  do j = 0, n3 - 1
    call core(n1, z(0:n1-1,i,j), dir)
  enddo
enddo
do i = 0, n1 - 1
  do j = 0, n3 - 1
    call core(n2, z(i,0:n2-1,j), dir)
  enddo
enddo
do i = 0, n1 - 1
  do j = 0, n2 - 1
    call core(n3, z(i,j,0:n3-1), dir)
  enddo
enddo
if (dir == -1) then
  z = z / dcmplx(n1 * n2 * n3, 0)
endif
return
endsubroutine

recursive subroutine core(n, z, dir)
implicit none
real*8    , parameter     :: dpi = 6.2831853071795864769252d0
integer   , intent(in)    :: n, dir
complex*16, intent(inout) :: z(0:n-1)
complex*16 t(0:n-1)
complex*16 wn, wp
complex*16 wnt(0:n-1)
complex*16 tot
integer    p, np
integer    i, j, k

if (n == 1) then
  return
elseif (iand(n, 1) == 0) then
  p = 2
  np = n / 2
else
  do p = 3, n, 2
    np = n / p
    if (n == np * p) exit
  enddo
endif

do i = 0, n - 1
  wnt(i) = cdexp(dcmplx(0.0d0, dpi * dble(i) / dble(sign(n, dir))))
enddo

do i = 0, np - 1
  do j = 0, p - 1
    tot = dcmplx(0.0d0, 0.0d0)
    do k = 0, p - 1
      tot = tot + z(i + np * k) * wnt(mod(j * k * np, n))
    enddo
    t(i + np * j) = tot * wnt(mod(i * j, n))
  enddo
enddo

do j = 0, p - 1
  call core(np, t(np * j: np * j - 1), dir)
enddo

do i = 0, np - 1
  do j = 0, p - 1
    z(p * i + j) = t(i + np * j)
  enddo
enddo

return
endsubroutine
