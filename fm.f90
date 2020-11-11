subroutine setup_fm
implicit none
include 'pme.inc'
real*8             :: b(0:p_spl,0:p_spl,0:p_spl)
integer            :: ncr(0:p_spl,0:p_spl) ! nCr = ncr(r, n)
real*8             :: per_fact(0:p_spl)        ! (1/n!)
real*8             :: shifted_m_pow(0:p_spl - 1,0:p_spl-1)
integer            :: i, j, k, m, n
logical, parameter :: idbg = .false.
common /fm_factor/b
intrinsic dble

! make pascal's triangle (ncr(r, n) = nCr)
ncr(0, 0) = 1
do i = 1, p_spl
  ncr(0,i) = 1
  do j = 1, i - 1
    ncr(j,i) = ncr(j,i-1) + ncr(j-1,i-1)
  enddo
  ncr(i,i) = 1
enddo

per_fact(0) = 1.0d0
do i = 1, p_spl
  per_fact(i) = per_fact(i - 1) / dble(i)
enddo

do m = 0, p_spl - 1
  shifted_m_pow(m, 0) = 1.0d0
  shifted_m_pow(m, 1) = (dble(p_spl)*0.5d0 - m)
  do n = 2, p_spl - 1
    shifted_m_pow(m, n) = shifted_m_pow(m, n - 1) * shifted_m_pow(m, 1)
  enddo
enddo

!! make b-table
!! shifted_m_pow(m, n) = (dble(p_spl)*0.5d0 - m) ** n
!! fm(n,u) = sum_(j=0,k=0)^(n-1,[u]) b(j,k,n) * (u-p_spl/2) ** j
!! u**j can be too large and inaccurate, fm is expanded around u-p_spl/2.
b = 0.0d0
do n = p_spl - 1, p_spl
  do j = 0, n - 1
    b(j, 0, n) = ncr(j, n-1) * shifted_m_pow(0, n-1-j)
  enddo
enddo
do n = p_spl - 1, p_spl
  do j = 0, n - 1
    do m = 1, n - 1
      b(j, m, n) = b(j, m - 1, n) + ncr(j, n-1) * shifted_m_pow(m, n-1-j) * (-1)**m * ncr(m, n)
    enddo
  enddo
enddo
do n = p_spl - 1, p_spl
  b(:, :, n) = b(:, :, n) * per_fact(n-1)
enddo

return
endsubroutine

real*8 function fm(n,u)
implicit none
include 'pme.inc'
integer, intent(in)  :: n
real*8 , intent(in)  :: u
integer              :: j, m
real*8               :: b(0:p_spl,0:p_spl,0:p_spl)
real*8               :: shifted_u
common /fm_factor/b

m = int(u)
shifted_u = u - dble(p_spl) * 0.5d0
fm = b(n - 1, m, n)
do j = n - 2, 0, -1
  fm = fm * shifted_u + b(j, m, n)
enddo
return
endfunction
