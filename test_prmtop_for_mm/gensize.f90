program gensize
implicit none
integer      :: NATOM,    NTYPES, NBONH,  MBONA,  NTHETH, MTHETA, NPHIH,  MPHIA,  NHPARM, NPARM
integer      :: NNB,      NRES,   NBONA,  NTHETA, NPHIA,  NUMBND, NUMANG, NPTRA,  NATYP,  NPHB
integer      :: IFPERT,   NBPER,  NGPER,  NDPER,  MBPER,  MGPER,  MDPER,  IFBOX,  NMXRS,  IFCAP
integer      :: NUMEXTRA, NCOPY
integer      :: eof
character*80 :: line

eof = 0
do 
  read(*,'(a80)',iostat=eof) line
  !
  !   Emergency exit
  !
  if(eof/=0) exit
  !
  !   Start reading
  !
  if(line(1:len_trim(line))=='%FLAG POINTERS') then
    read(*,*) ! Skip '%FORMAT' Line
    read(*,'(10i8)') NATOM,    NTYPES, NBONH,  MBONA,  NTHETH, MTHETA, NPHIH,  MPHIA,  NHPARM, NPARM
    read(*,'(10i8)') NNB,      NRES,   NBONA,  NTHETA, NPHIA,  NUMBND, NUMANG, NPTRA,  NATYP,  NPHB
    read(*,'(10i8)') IFPERT,   NBPER,  NGPER,  NDPER,  MBPER,  MGPER,  MDPER,  IFBOX,  NMXRS,  IFCAP
    read(*,'(10i8)') NUMEXTRA, NCOPY
  !
  !   Stop reading.
  !
  elseif(line(1:len_trim(line))=='%FLAG ATOM_NAME') then    
    write(*,'(a)') '! Prmtop parameter size definition'
    write(*,'(a,i8,a)') 'integer,parameter :: m_natom  = ', natom , '    ! number of max atom                   '
    write(*,'(a,i8,a)') 'integer,parameter :: m_ntype  = ', ntypes, '    ! number of atomtype                   '
    write(*,'(a,i8,a)') 'integer,parameter :: m_nres   = ', nres  , '    ! number of residue                    '
    write(*,'(a,i8,a)') 'integer,parameter :: m_numbnd = ', numbnd, '    ! number of bond                       '
    write(*,'(a,i8,a)') 'integer,parameter :: m_numang = ', numang, '    ! number of angle                      '
    write(*,'(a,i8,a)') 'integer,parameter :: m_nptra  = ', nptra , '    ! number of dihedral                   '
    write(*,'(a,i8,a)') 'integer,parameter :: m_nbonh  = ', nbonh , '    ! number of bond (including H)         '
    write(*,'(a,i8,a)') 'integer,parameter :: m_nbona  = ', nbona , '    ! number of bond (without H)           '
    write(*,'(a,i8,a)') 'integer,parameter :: m_ntheth = ', ntheth, '    ! number of angle (including H)        '
    write(*,'(a,i8,a)') 'integer,parameter :: m_ntheta = ', ntheta, '    ! number of angle (without H)          '
    write(*,'(a,i8,a)') 'integer,parameter :: m_nphih  = ', nphih , '    ! number of dihedral (including H)     '
    write(*,'(a,i8,a)') 'integer,parameter :: m_nphia  = ', nphia , '    ! number of dihedral (including H)     '
    write(*,'(a,i8,a)') 'integer,parameter :: m_nnb    = ', nnb   , '    ! number of nonbon                     '
    write(*,'(a,i8,a)') 'integer,parameter :: m_nphb   = ', nphb  , '    ! number of 10-12 hydrogen bond types  '
    write(*,'(a,i8,a)') 'integer,parameter :: m_natyp  = ', natyp , '    ! number of atomtypes in parameter file'
    stop
  endif
enddo
endprogram
