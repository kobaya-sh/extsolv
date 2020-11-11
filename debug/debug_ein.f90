program debug_ein
integer :: deriv_req
character*60 :: filename
call getarg(1,filename)
call ein(filename,deriv_req)
endprogram
