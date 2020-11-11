program debug_function
implicit none

real*8          :: n(3)
!Functions
real*8,external :: length
real*8,external :: angle
real*8,external :: dihe

!r=1.0
write(0,*) length(       &
  (/0.0d0,0.0d0,0.0d0/), &
  (/1.0d0,0.0d0,0.0d0/)  &
)
!a=pi/4
write(0,*) angle(        &
  (/0.0d0,0.0d0,0.0d0/), &
  (/0.0d0,0.0d0,1.0d0/), &
  (/0.0d0,1.0d0,0.0d0/)  &
)
!d=pi/4
write(0,*) dihe(         &
  (/0.0d0,0.0d0,0.0d0/), &
  (/1.0d0,0.0d0,0.0d0/), &
  (/1.0d0,0.0d0,1.0d0/), &
  (/0.0d0,1.0d0,1.0d0/)  &
)

call cross((/1.0d0,0.0d0,0.0d0/),(/0.0d0,1.0d0,0.0d0/),n)
write(0,*) n(1),n(2),n(3)

write(0,*) 'erfc(1.0d0)  ',erfc(1.0d0)
write(0,*) 'erfc(7.5d-1) ',erfc(7.5d-1)
write(0,*) 'erfc(5.0d-1) ',erfc(5.0d-1)
write(0,*) 'erfc(2.5d-1) ',erfc(2.5d-1)
write(0,*) 'erfc(1.0d-1) ',erfc(1.0d-1)
write(0,*) 'erfc(1.0d-3) ',erfc(1.0d-3)
write(0,*) 'erfc(1.0d-6) ',erfc(1.0d-6)
write(0,*) 'erfc(1.0d-12)',erfc(1.0d-12)
endprogram
