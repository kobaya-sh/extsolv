program debug_option
character*60 :: prmfile_name, log_name, optfile_name
integer      :: log_level
integer      :: fh_log
integer      :: i, j
include 'mmpara.inc'
optfile_name = 'T'
log_level = 0
fh_log = 10
call option ( prmfile_name, log_name, optfile_name, log_level, fh_log )
write(0,*) 'prmfile_name = ', trim(prmfile_name)
write(0,*) 'log_name     = ', trim(log_name)
write(0,*) 'optfile_name = ', trim(optfile_name)
write(0,*) 'log_level    = ', log_level
write(0,*) 'fh_log       = ', fh_log
write(0,*) 'is_pbc       = ', is_pbc
write(0,*) 'is_ewald     = ', is_ewald
write(0,*)
write(0,*) 'cell         = ', cell
do i=1,3
  write(0,*) 'basis_r      = ', (basis_r(i,j),j=1,3)
enddo
do i=1,3
  write(0,*) 'basis_k      = ', (basis_k(i,j),j=1,3)
enddo
stop
endprogram
