program test
integer i, largek1
largek1 = 32
do i_k = 0, largek1-1
  write(*,*) dble ( i_k - ( 2 * i_k / largek1 ) * largek1 )
enddo
end program

