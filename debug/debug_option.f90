program debug_option
character*60 :: prmfile_name, log_name, optfile_name
optfile_name = 'T'
call option ( prmfile_name, log_name, optfile_name )
stop
endprogram
