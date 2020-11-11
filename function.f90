real*8 function length(ri,rj)    
implicit none
real*8       :: ri(3),rj(3)
real*8       :: rij(3)
real*8,external :: length_vec
rij = rj-ri
length=length_vec(rij)
!length=length_vec(rj-ri)
endfunction

real*8 function angle(ri,rj,rk) 
implicit none
real*8       :: ri(3),rj(3),rk(3)
real*8,external :: angle_vec
angle=angle_vec(ri-rj,rk-rj)
endfunction

real*8 function dihe(ri,rj,rk,rl)  !D_ijkl
implicit none
real*8       :: ri(3),rj(3),rk(3),rl(3)
real*8       :: nji(3),nkl(3)
real*8       :: lkj
integer :: i
intrinsic dot_product
real*8,external :: length_vec,angle_vec
nji=(ri-rj)-(rk-rj)*dot_product(ri-rj,rk-rj)/length_vec(rk-rj)**2
nkl=(rl-rk)-(rj-rk)*dot_product(rl-rk,rj-rk)/length_vec(rj-rk)**2
if ( length_vec(nji) < 1.0d-10 .or. length_vec(nkl) < 1.0d-10 ) then
  dihe = 0.0d0
else
  dihe=angle_vec(nji,nkl)
endif
endfunction

real*8 function length_vec(rji)    !R_ij
implicit none
real*8       :: rji(3)
intrinsic dsqrt
length_vec=dsqrt(rji(1)**2+rji(2)**2+rji(3)**2)
endfunction

real*8 function angle_vec(rij,rkj) !A_ijk
implicit none
real*8       :: rij(3),rkj(3)
real*8       :: cos_theta 
intrinsic dacos,dot_product
real*8,external :: length_vec
cos_theta = dot_product(rij,rkj)/(length_vec(rij)*length_vec(rkj))
if ( cos_theta > 0.99999999999999d0 ) then
  angle_vec = 0.0d0
elseif ( cos_theta < -0.99999999999999d0 ) then
  angle_vec = 3.1415926535897932384d0
else
  angle_vec=dacos(cos_theta)
endif
endfunction

subroutine cross(a,b,n) !n=a(cross)b
implicit none
real*8,intent(in)  :: a(3),b(3)
real*8,intent(out) :: n(3)
n(1)=a(2)*b(3)-a(3)*b(2)
n(2)=a(3)*b(1)-a(1)*b(3)
n(3)=a(1)*b(2)-a(2)*b(1)
endsubroutine

real*8 function erfc(x)
!
!   Reference:
!
!   Abramowitz & Stegun Handbook of Mathematical functions
!   National Bureau of Standards, Formula 7.1.26
!
!   This Formula is used in Allen & Tildesley 
!
implicit none
real*8            :: x
real*8            :: t
real*8, parameter :: p  =  0.3275911d0 
real*8, parameter :: a1 =  0.254829592d0
real*8, parameter :: a2 = -0.284496736d0
real*8, parameter :: a3 =  1.421413741d0
real*8, parameter :: a4 = -1.453152027d0
real*8, parameter :: a5 =  1.061405429d0
intrinsic dexp
t = 1.0d0 / (1.0d0 + p * x)
erfc = (((((a5 * t) + a4) * t + a3) * t + a2) * t + a1) * t &
     * dexp(-(x ** 2))
return
endfunction

real*8 function erf(x)
implicit none
real*8          :: x
real*8,external :: erfc
erf=1.0d0-erfc(x)
return
endfunction

real*8 function det3(a)
implicit none
real*8, intent(in)  :: a(3, 3)
det3 = a(1, 1) * (a(2, 2) * a(3, 3) - a(3, 2) * a(2, 3)) &
     + a(2, 1) * (a(3, 2) * a(1, 3) - a(1, 2) * a(3, 3)) &
     + a(3, 1) * (a(1, 2) * a(2, 3) - a(2, 2) * a(1, 3))
return
endfunction

real*8 function triplep(a, b, c)
implicit none
real*8, intent(in)  :: a(3), b(3), c(3)
triplep = a(1) * (b(2) * c(3) - b(3) * c(2)) &
        + a(2) * (b(3) * c(1) - b(1) * c(3)) &
        + a(3) * (b(1) * c(2) - b(2) * c(1))
return
endfunction
