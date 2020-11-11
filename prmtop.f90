subroutine prmtop(prmfile_name,           &
  ITITL,                                  &
  NATOM,  NTYPES, NBONH,  MBONA,  NTHETH, &
  MTHETA, NPHIH,  MPHIA,  NHPARM, NPARM,  &
  NNB,    NRES,   NBONA,  NTHETA, NPHIA,  &
  NUMBND, NUMANG, NPTRA,  NATYP,  NPHB,   &
  IFPERT, NBPER,  NGPER,  NDPER,  MBPER,  &
  MGPER,  MDPER,  IFBOX,  NMXRS,  IFCAP,  &
  NUMEXTRA, NCOPY,                        &
  IGRAPH,                                 &
  CHARGE,                                 &
  AMASS,                                  &
  IAC,                                    &
  NUMEX,                                  &
  ICO,                                    &
  LBRES, IPRES,                           &
  IJICBH, IJICB, IJKICTH, IJKICT,         &
  IJKLICPH, IJKLICP,                      &
  INB,                                    &
  ISYMBL,                                 &
  ITREE,                                  &
  TYPE,                                   &
  RK, REQ, TK, TEQ, PK,  PN,  PHASE,      &
  ONE_SCEE, ONE_SCNB, SOLTY,              &
  CN1,CN2,                                &
  ASOL, BSOL,                             &
  RBORN,                                  &
  FS                                      &
)
implicit none
include 'mmcounter.inc'
include 'size.inc'
include 'prmtop.inc'

! files
integer,parameter       :: fh_prmtop = 7
character*60,intent(in) :: prmfile_name
! User difinition
integer                 :: nitem !item_per_line

idbg = .false.
!idbg = .true.
eof = 0
!
!    0. Defaults
!
ONE_SCEE=1.0d0/1.2d0
ONE_SCNB=1.0d0/2.0d0
!
!    1. Read prmtops
!
open(fh_prmtop,file=prmfile_name)
do 
  read(fh_prmtop,'(a80)',iostat=eof) line
  if(eof/=0) exit
  if(line(1:len_trim(line))=='%FLAG TITLE') then
    read(fh_prmtop,*) line
    read(fh_prmtop,'(a80)') ITITL
    if(idbg) then
      write(*,'(a)') '%FLAG TITLE'
      write(*,'(a)') '%FORMAT(20a4)'
      write(*,*) ITITL
    endif
  elseif(line(1:len_trim(line))=='%FLAG POINTERS') then
    read(fh_prmtop,'(a80)') line
    fmt = line(8:len_trim(line))
    read(fh_prmtop,fmt) NATOM,    NTYPES, NBONH,  MBONA,  NTHETH, MTHETA, NPHIH,  MPHIA,  NHPARM, NPARM
    read(fh_prmtop,fmt) NNB,      NRES,   NBONA,  NTHETA, NPHIA,  NUMBND, NUMANG, NPTRA,  NATYP,  NPHB
    read(fh_prmtop,fmt) IFPERT,   NBPER,  NGPER,  NDPER,  MBPER,  MGPER,  MDPER,  IFBOX,  NMXRS,  IFCAP
    read(fh_prmtop,fmt) NUMEXTRA, NCOPY
    if(idbg)then
      write(*,'(a)') '%FLAG POINTERS'
      write(*,'(a)') '%FORMAT'//fmt
      write(*,fmt) NATOM,    NTYPES, NBONH,  MBONA,  NTHETH, MTHETA, NPHIH,  MPHIA,  NHPARM, NPARM
      write(*,fmt) NNB,      NRES,   NBONA,  NTHETA, NPHIA,  NUMBND, NUMANG, NPTRA,  NATYP,  NPHB
      write(*,fmt) IFPERT,   NBPER,  NGPER,  NDPER,  MBPER,  MGPER,  MDPER,  IFBOX,  NMXRS,  IFCAP
      write(*,fmt) NUMEXTRA, NCOPY
    endif
  elseif(line(1:len_trim(line))=='%FLAG ATOM_NAME') then
    nitem=20
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NATOM/nitem)-1
      read(fh_prmtop,fmt) (IGRAPH(nitem*i+j),j=1,nitem)
    enddo
    if(mod(natom,nitem)>0)then
      read(fh_prmtop,fmt) (IGRAPH(nitem*i+j),j=1,mod(natom,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG ATOMNAME'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NATOM/nitem)-1
        write(*,fmt) (IGRAPH(nitem*i+j),j=1,nitem)
      enddo
      if(mod(natom,nitem)>0)then
        write(*,fmt) (IGRAPH(nitem*i+j),j=1,mod(natom,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG CHARGE') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NATOM/nitem)-1
      read(fh_prmtop,fmt) (CHARGE(nitem*i+j),j=1,nitem)
    enddo
    if(mod(natom,nitem)>0)then
      read(fh_prmtop,fmt) (CHARGE(nitem*i+j),j=1,mod(natom,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG CHARGE'
      write(*,'(a)') '%FORMAT'//fmt 
      do i=0,(NATOM/nitem)-1
        write(*,fmt) (CHARGE(nitem*i+j),j=1,nitem)
      enddo
      if(mod(natom,nitem)>0)then
        write(*,fmt) (CHARGE(nitem*i+j),j=1,mod(natom,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG MASS') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NATOM/nitem)-1
      read(fh_prmtop,fmt) (AMASS(nitem*i+j),j=1,nitem)
    enddo
    if(mod(natom,nitem)>0)then
      read(fh_prmtop,fmt) (AMASS(nitem*i+j),j=1,mod(natom,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG MASS'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NATOM/nitem)-1
        write(*,fmt) (AMASS(nitem*i+j),j=1,nitem)
      enddo
      if(mod(natom,nitem)>0)then
        write(*,fmt) (AMASS(nitem*i+j),j=1,mod(natom,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG ATOM_TYPE_INDEX') then
    nitem=10
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NATOM/nitem)-1
      read(fh_prmtop,fmt) (IAC(nitem*i+j),j=1,nitem)
    enddo
    if(mod(natom,nitem)>0)then
      read(fh_prmtop,fmt) (IAC(nitem*i+j),j=1,mod(natom,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG ATOM_TYPE_INDEX'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NATOM/nitem)-1
        write(*,fmt) (IAC(nitem*i+j),j=1,nitem)
      enddo
      if(mod(natom,nitem)>0)then
        write(*,fmt) (IAC(nitem*i+j),j=1,mod(natom,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG NUMBER_EXCLUDED_ATOMS') then
    nitem=10
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NATOM/nitem)-1
      read(fh_prmtop,fmt) (NUMEX(nitem*i+j),j=1,nitem)
    enddo
    if(mod(natom,nitem)>0)then
      read(fh_prmtop,fmt) (NUMEX(nitem*i+j),j=1,mod(natom,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG NUMBER_EXCLUDED_ATOMS'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NATOM/nitem)-1
        write(*,fmt) (NUMEX(nitem*i+j),j=1,nitem)
      enddo
      if(mod(natom,nitem)>0)then
        write(*,fmt) (NUMEX(nitem*i+j),j=1,mod(natom,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG NONBONDED_PARM_INDEX') then
    nitem=10
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NTYPES**2/nitem)-1
      read(fh_prmtop,fmt) (ICO(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NTYPES**2,nitem)>0)then
      read(fh_prmtop,fmt) (ICO(nitem*i+j),j=1,mod(NTYPES**2,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG NONBONDED_PARM_INDEX'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NTYPES**2/nitem)-1
        write(*,fmt) (ICO(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NTYPES**2,nitem)>0)then
        write(*,fmt) (ICO(nitem*i+j),j=1,mod(NTYPES**2,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG RESIDUE_LABEL') then
    nitem=20
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NRES/nitem)-1
      read(fh_prmtop,fmt) (LBRES(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NRES,nitem)>0)then
      read(fh_prmtop,fmt) (LBRES(nitem*i+j),j=1,mod(NRES,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG RESIDUE_LABEL'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NRES/nitem)-1
        write(*,fmt) (LBRES(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NRES,nitem)>0)then
        write(*,fmt) (LBRES(nitem*i+j),j=1,mod(NRES,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG RESIDUE_POINTER') then
    nitem=10
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NRES/nitem)-1
      read(fh_prmtop,fmt) (IPRES(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NRES,nitem)>0)then
      read(fh_prmtop,fmt) (IPRES(nitem*i+j),j=1,mod(NRES,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG RESIDUE_POINTER'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NRES/nitem)-1
        write(*,fmt) (IPRES(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NRES,nitem)>0)then
        write(*,fmt) (IPRES(nitem*i+j),j=1,mod(NRES,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG RESIDUE_POINTER') then
    nitem=10
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NRES/nitem)-1
      read(fh_prmtop,fmt) (IPRES(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NRES,nitem)>0)then
      read(fh_prmtop,fmt) (IPRES(nitem*i+j),j=1,mod(NRES,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG RESIDUE_POINTER'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NRES/nitem)-1
        write(*,fmt) (IPRES(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NRES,nitem)>0)then
        write(*,fmt) (IPRES(nitem*i+j),j=1,mod(NRES,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG BOND_FORCE_CONSTANT') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NUMBND/nitem)-1
      read(fh_prmtop,fmt) (RK(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NUMBND,nitem)>0)then
      read(fh_prmtop,fmt) (RK(nitem*i+j),j=1,mod(NUMBND,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG BOND_FORCE_CONSTANT'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NUMBND/nitem)-1
        write(*,fmt) (RK(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NUMBND,nitem)>0)then
        write(*,fmt) (RK(nitem*i+j),j=1,mod(NUMBND,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG BOND_EQUIL_VALUE') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NUMBND/nitem)-1
      read(fh_prmtop,fmt) (REQ(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NUMBND,nitem)>0)then
      read(fh_prmtop,fmt) (REQ(nitem*i+j),j=1,mod(NUMBND,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG BOND_FORCE_CONSTANT'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NUMBND/nitem)-1
        write(*,fmt) (REQ(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NUMBND,nitem)>0)then
        write(*,fmt) (REQ(nitem*i+j),j=1,mod(NUMBND,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG ANGLE_FORCE_CONSTANT') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NUMANG/nitem)-1
      read(fh_prmtop,fmt) (TK(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NUMANG,nitem)>0)then
      read(fh_prmtop,fmt) (TK(nitem*i+j),j=1,mod(NUMANG,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG ANGLE_FORCE_CONSTANT'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NUMANG/nitem)-1
        write(*,fmt) (TK(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NUMANG,nitem)>0)then
        write(*,fmt) (TK(nitem*i+j),j=1,mod(NUMANG,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG ANGLE_EQUIL_VALUE') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NUMANG/nitem)-1
      read(fh_prmtop,fmt) (TEQ(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NUMANG,nitem)>0)then
      read(fh_prmtop,fmt) (TEQ(nitem*i+j),j=1,mod(NUMANG,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG ANGLE_FORCE_CONSTANT'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NUMANG/nitem)-1
        write(*,fmt) (TEQ(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NUMANG,nitem)>0)then
        write(*,fmt) (TEQ(nitem*i+j),j=1,mod(NUMANG,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG DIHEDRAL_FORCE_CONSTANT') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NPTRA/nitem)-1
      read(fh_prmtop,fmt) (PK(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NPTRA,nitem)>0)then
      read(fh_prmtop,fmt) (PK(nitem*i+j),j=1,mod(NPTRA,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG DIHEDRAL_FORCE_CONSTANT'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NPTRA/nitem)-1
        write(*,fmt) (PK(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NPTRA,nitem)>0)then
        write(*,fmt) (PK(nitem*i+j),j=1,mod(NPTRA,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG DIHEDRAL_PERIODICITY') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NPTRA/nitem)-1
      read(fh_prmtop,fmt) (PN(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NPTRA,nitem)>0)then
      read(fh_prmtop,fmt) (PN(nitem*i+j),j=1,mod(NPTRA,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG DIHEDRAL_PERIODICITY'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NPTRA/nitem)-1
        write(*,fmt) (PN(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NPTRA,nitem)>0)then
        write(*,fmt) (PN(nitem*i+j),j=1,mod(NPTRA,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG DIHEDRAL_PHASE') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NPTRA/nitem)-1
      read(fh_prmtop,fmt) (PHASE(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NPTRA,nitem)>0)then
      read(fh_prmtop,fmt) (PHASE(nitem*i+j),j=1,mod(NPTRA,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG DIHEDRAL_PHASE'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NPTRA/nitem)-1
        write(*,fmt) (PHASE(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NPTRA,nitem)>0)then
        write(*,fmt) (PHASE(nitem*i+j),j=1,mod(NPTRA,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG SCEE_SCALE_FACTOR') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NPTRA/nitem)-1
      read(fh_prmtop,fmt) (ONE_SCEE(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NPTRA,nitem)>0)then
      read(fh_prmtop,fmt) (ONE_SCEE(nitem*i+j),j=1,mod(NPTRA,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG DIHEDRAL_SCALE_FACTOR'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NPTRA/nitem)-1
        write(*,fmt) (ONE_SCEE(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NPTRA,nitem)>0)then
        write(*,fmt) (ONE_SCEE(nitem*i+j),j=1,mod(NPTRA,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG SCNB_SCALE_FACTOR') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NPTRA/nitem)-1
      read(fh_prmtop,fmt) (ONE_SCNB(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NPTRA,nitem)>0)then
      read(fh_prmtop,fmt) (ONE_SCNB(nitem*i+j),j=1,mod(NPTRA,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG DIHEDRAL_SCALE_FACTOR'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NPTRA/nitem)-1
        write(*,fmt) (ONE_SCNB(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NPTRA,nitem)>0)then
        write(*,fmt) (ONE_SCNB(nitem*i+j),j=1,mod(NPTRA,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG SOLTY') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NATYP/nitem)-1
      read(fh_prmtop,fmt) (SOLTY(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NATYP,nitem)>0)then
      read(fh_prmtop,fmt) (SOLTY(nitem*i+j),j=1,mod(NATYP,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG SOLTY'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NATYP/nitem)-1
        write(*,fmt) (SOLTY(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NATYP,nitem)>0)then
        write(*,fmt) (SOLTY(nitem*i+j),j=1,mod(NATYP,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG LENNARD_JONES_ACOEF') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NTYPES*(NTYPES+1)/2/nitem)-1
      read(fh_prmtop,fmt) (CN1(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NTYPES*(NTYPES+1)/2,nitem)>0)then
      read(fh_prmtop,fmt) (CN1(nitem*i+j),j=1,mod(NTYPES*(NTYPES+1)/2,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG LENNARD_JONES_ACOEF'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NTYPES*(NTYPES+1)/2/nitem)-1
        write(*,fmt) (CN1(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NTYPES*(NTYPES+1)/2,nitem)>0)then
        write(*,fmt) (CN1(nitem*i+j),j=1,mod(NTYPES*(NTYPES+1)/2,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG LENNARD_JONES_BCOEF') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NTYPES*(NTYPES+1)/2/nitem)-1
      read(fh_prmtop,fmt) (CN2(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NTYPES*(NTYPES+1)/2,nitem)>0)then
      read(fh_prmtop,fmt) (CN2(nitem*i+j),j=1,mod(NTYPES*(NTYPES+1)/2,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG LENNARD_JONES_BCOEF'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NTYPES*(NTYPES+1)/2/nitem)-1
        write(*,fmt) (CN2(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NTYPES*(NTYPES+1)/2,nitem)>0)then
        write(*,fmt) (CN2(nitem*i+j),j=1,mod(NTYPES*(NTYPES+1)/2,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG BONDS_INC_HYDROGEN') then
    nitem=10
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(3*NBONH/nitem)-1
      read(fh_prmtop,fmt) (IJICBH(nitem*i+j),j=1,nitem)
    enddo
    if(mod(3*NBONH,nitem)>0)then
      read(fh_prmtop,fmt) (IJICBH(nitem*i+j),j=1,mod(3*NBONH,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG BONDS_INC_HYDROGEN'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(3*NBONH/nitem)-1
        write(*,fmt) (IJICBH(nitem*i+j),j=1,nitem)
      enddo
      if(mod(3*NBONH,nitem)>0)then
        write(*,fmt) (IJICBH(nitem*i+j),j=1,mod(3*NBONH,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG BONDS_WITHOUT_HYDROGEN') then
    nitem=10
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(3*NBONA/nitem)-1
      read(fh_prmtop,fmt) (IJICB(nitem*i+j),j=1,nitem)
    enddo
    if(mod(3*NBONA,nitem)>0)then
      read(fh_prmtop,fmt) (IJICB(nitem*i+j),j=1,mod(3*NBONA,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG BONDS_WITHOUT_HYDROGEN'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(3*NBONA/nitem)-1
        write(*,fmt) (IJICB(nitem*i+j),j=1,nitem)
      enddo
      if(mod(3*NBONA,nitem)>0)then
        write(*,fmt) (IJICB(nitem*i+j),j=1,mod(3*NBONA,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG ANGLES_INC_HYDROGEN') then
    nitem=10
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(4*NTHETH/nitem)-1
      read(fh_prmtop,fmt) (IJKICTH(nitem*i+j),j=1,nitem)
    enddo
    if(mod(4*NTHETH,nitem)>0)then
      read(fh_prmtop,fmt) (IJKICTH(nitem*i+j),j=1,mod(4*NTHETH,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG ANGLES_INC_HYDROGEN'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(4*NTHETH/nitem)-1
        write(*,fmt) (IJKICTH(nitem*i+j),j=1,nitem)
      enddo
      if(mod(4*NTHETH,nitem)>0)then
        write(*,fmt) (IJKICTH(nitem*i+j),j=1,mod(4*NTHETH,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG ANGLES_WITHOUT_HYDROGEN') then
    nitem=10
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(4*NTHETA/nitem)-1
      read(fh_prmtop,fmt) (IJKICT(nitem*i+j),j=1,nitem)
    enddo
    if(mod(4*NTHETA,nitem)>0)then
      read(fh_prmtop,fmt) (IJKICT(nitem*i+j),j=1,mod(4*NTHETA,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG ANGLES_WITHOUT_HYDROGEN'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(4*NTHETA/nitem)-1
        write(*,fmt) (IJKICT(nitem*i+j),j=1,nitem)
      enddo
      if(mod(4*NTHETA,nitem)>0)then
        write(*,fmt) (IJKICT(nitem*i+j),j=1,mod(4*NTHETA,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG DIHEDRALS_INC_HYDROGEN') then
    nitem=10
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(5*NPHIH/nitem)-1
      read(fh_prmtop,fmt) (IJKLICPH(nitem*i+j),j=1,nitem)
    enddo
    if(mod(5*NPHIH,nitem)>0)then
      read(fh_prmtop,fmt) (IJKLICPH(nitem*i+j),j=1,mod(5*NPHIH,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG DIHEDRALS_INC_HYDROGEN'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(5*NPHIH/nitem)-1
        write(*,fmt) (IJKLICPH(nitem*i+j),j=1,nitem)
      enddo
      if(mod(5*NPHIH,nitem)>0)then
        write(*,fmt) (IJKLICPH(nitem*i+j),j=1,mod(5*NPHIH,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG DIHEDRALS_WITHOUT_HYDROGEN') then
    nitem=10
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(5*NPHIA/nitem)-1
      read(fh_prmtop,fmt) (IJKLICP(nitem*i+j),j=1,nitem)
    enddo
    if(mod(5*NPHIA,nitem)>0)then
      read(fh_prmtop,fmt) (IJKLICP(nitem*i+j),j=1,mod(5*NPHIA,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG DIHEDRALS_WITHOUT_HYDROGEN'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(5*NPHIA/nitem)-1
        write(*,fmt) (IJKLICP(nitem*i+j),j=1,nitem)
      enddo
      if(mod(5*NPHIA,nitem)>0)then
        write(*,fmt) (IJKLICP(nitem*i+j),j=1,mod(5*NPHIA,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG EXCLUDED_ATOMS_LIST') then
    nitem=10
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NNB/nitem)-1
      read(fh_prmtop,fmt) (INB(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NNB,nitem)>0)then
      read(fh_prmtop,fmt) (INB(nitem*i+j),j=1,mod(NNB,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG EXCLUDED_ATOMS_LIST'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NNB/nitem)-1
        write(*,fmt) (INB(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NNB,nitem)>0)then
        write(*,fmt) (INB(nitem*i+j),j=1,mod(NNB,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG HBOND_ACOEF') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NPHB/nitem)-1
      read(fh_prmtop,fmt) (ASOL(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NPHB,nitem)>0)then
      read(fh_prmtop,fmt) (ASOL(nitem*i+j),j=1,mod(NPHB,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG HBOND_ACOEF'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NPHB/nitem)-1
        write(*,fmt) (ASOL(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NPHB,nitem)>0)then
        write(*,fmt) (ASOL(nitem*i+j),j=1,mod(NPHB,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG HBOND_BCOEF') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NPHB/nitem)-1
      read(fh_prmtop,fmt) (BSOL(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NPHB,nitem)>0)then
      read(fh_prmtop,fmt) (BSOL(nitem*i+j),j=1,mod(NPHB,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG HBOND_BCOEF'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NPHB/nitem)-1
        write(*,fmt) (BSOL(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NPHB,nitem)>0)then
        write(*,fmt) (BSOL(nitem*i+j),j=1,mod(NPHB,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG AMBER_ATOM_TYPE') then
    nitem=20
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NATOM/nitem)-1
      read(fh_prmtop,fmt) (ISYMBL(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NATOM,nitem)>0)then
      read(fh_prmtop,fmt) (ISYMBL(nitem*i+j),j=1,mod(NATOM,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG AMBER_ATOM_TYPE'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NATOM/nitem)-1
        write(*,fmt) (ISYMBL(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NATOM,nitem)>0)then
        write(*,fmt) (ISYMBL(nitem*i+j),j=1,mod(NATOM,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG TREE_CHAIN_CLASSIFICATION') then
    nitem=20
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NATOM/nitem)-1
      read(fh_prmtop,fmt) (ITREE(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NATOM,nitem)>0)then
      read(fh_prmtop,fmt) (ITREE(nitem*i+j),j=1,mod(NATOM,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG AMBER_ATOM_TYPE'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NATOM/nitem)-1
        write(*,fmt) (ITREE(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NATOM,nitem)>0)then
        write(*,fmt) (ITREE(nitem*i+j),j=1,mod(NATOM,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG RADII') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NATOM/nitem)-1
      read(fh_prmtop,fmt) (RBORN(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NATOM,nitem)>0)then
      read(fh_prmtop,fmt) (RBORN(nitem*i+j),j=1,mod(NATOM,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG RADII'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NATOM/nitem)-1
        write(*,fmt) (RBORN(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NATOM,nitem)>0)then
        write(*,fmt) (RBORN(nitem*i+j),j=1,mod(NATOM,nitem))
      endif
    endif
  elseif(line(1:len_trim(line))=='%FLAG SCREEN') then
    nitem=5
    read(fh_prmtop,*) line
    fmt = line(8:len_trim(line))
    do i=0,(NATOM/nitem)-1
      read(fh_prmtop,fmt) (FS(nitem*i+j),j=1,nitem)
    enddo
    if(mod(NATOM,nitem)>0)then
      read(fh_prmtop,fmt) (FS(nitem*i+j),j=1,mod(NATOM,nitem))
    endif
    if(idbg)then
      write(*,'(a)') '%FLAG SCREEN'
      write(*,'(a)') '%FORMAT'//fmt
      do i=0,(NATOM/nitem)-1
        write(*,fmt) (FS(nitem*i+j),j=1,nitem)
      enddo
      if(mod(NATOM,nitem)>0)then
        write(*,fmt) (FS(nitem*i+j),j=1,mod(NATOM,nitem))
      endif
    endif
  endif
enddo
close(fh_prmtop)
!
!    2. Debug Mode
!
idbg = .false.
!idbg = .true.

if(idbg) then
  write(*,'(a)') '%FLAG TITLE'
  write(*,'(a)') '%FORMAT(20a4)'
  write(*,*) ITITL
  fmt='(10I8)'
  nitem=10
  write(*,'(a)') '%FLAG POINTERS'
  write(*,'(a)') '%FORMAT'//fmt
  write(*,fmt) NATOM,    NTYPES, NBONH,  MBONA,  NTHETH, MTHETA, NPHIH,  MPHIA,  NHPARM, NPARM
  write(*,fmt) NNB,      NRES,   NBONA,  NTHETA, NPHIA,  NUMBND, NUMANG, NPTRA,  NATYP,  NPHB
  write(*,fmt) IFPERT,   NBPER,  NGPER,  NDPER,  MBPER,  MGPER,  MDPER,  IFBOX,  NMXRS,  IFCAP
  write(*,fmt) NUMEXTRA, NCOPY
  write(*,'(a)') '%FLAG ATOMNAME'
  write(*,'(a)') '%FORMAT'//fmt
  fmt='(20a4)'
  nitem=20
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (IGRAPH(nitem*i+j),j=1,nitem)
  enddo
  if(mod(natom,nitem)>0)then
    write(*,fmt) (IGRAPH(nitem*i+j),j=1,mod(natom,nitem))
  endif
  fmt='(5e16.8)'
  nitem=5
  write(*,'(a)') '%FLAG CHARGE'
  write(*,'(a)') '%FORMAT'//fmt 
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (CHARGE(nitem*i+j),j=1,nitem)
  enddo
  if(mod(natom,nitem)>0)then
    write(*,fmt) (CHARGE(nitem*i+j),j=1,mod(natom,nitem))
  endif
  write(*,'(a)') '%FLAG MASS'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (AMASS(nitem*i+j),j=1,nitem)
  enddo
  if(mod(natom,nitem)>0)then
    write(*,fmt) (AMASS(nitem*i+j),j=1,mod(natom,nitem))
  endif
  fmt='(10i8)'
  nitem=10
  write(*,'(a)') '%FLAG ATOM_TYPE_INDEX'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (IAC(nitem*i+j),j=1,nitem)
  enddo
  if(mod(natom,nitem)>0)then
    write(*,fmt) (IAC(nitem*i+j),j=1,mod(natom,nitem))
  endif
  write(*,'(a)') '%FLAG NUMBER_EXCLUDED_ATOMS'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (NUMEX(nitem*i+j),j=1,nitem)
  enddo
  if(mod(natom,nitem)>0)then
    write(*,fmt) (NUMEX(nitem*i+j),j=1,mod(natom,nitem))
  endif
  write(*,'(a)') '%FLAG NONBONDED_PARM_INDEX'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NTYPES**2/nitem)-1
    write(*,fmt) (ICO(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NTYPES**2,nitem)>0)then
    write(*,fmt) (ICO(nitem*i+j),j=1,mod(NTYPES**2,nitem))
  endif
  fmt='(20a4)'
  nitem=20
  write(*,'(a)') '%FLAG RESIDUE_LABEL'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NRES/nitem)-1
    write(*,fmt) (LBRES(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NRES,nitem)>0)then
    write(*,fmt) (LBRES(nitem*i+j),j=1,mod(NRES,nitem))
  endif
  fmt='(10i8)'
  nitem=10
  write(*,'(a)') '%FLAG RESIDUE_POINTER'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NRES/nitem)-1
    write(*,fmt) (IPRES(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NRES,nitem)>0)then
    write(*,fmt) (IPRES(nitem*i+j),j=1,mod(NRES,nitem))
  endif
  fmt='(5e16.8)'
  nitem=10
  write(*,'(a)') '%FLAG BOND_FORCE_CONSTANT'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NUMBND/nitem)-1
    write(*,fmt) (RK(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NUMBND,nitem)>0)then
    write(*,fmt) (RK(nitem*i+j),j=1,mod(NUMBND,nitem))
  endif
  write(*,'(a)') '%FLAG BOND_FORCE_EQUIL'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NUMBND/nitem)-1
    write(*,fmt) (REQ(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NUMBND,nitem)>0)then
    write(*,fmt) (REQ(nitem*i+j),j=1,mod(NUMBND,nitem))
  endif
  write(*,'(a)') '%FLAG ANGLE_FORCE_CONSTANT'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NUMANG/nitem)-1
    write(*,fmt) (TK(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NUMANG,nitem)>0)then
    write(*,fmt) (TK(nitem*i+j),j=1,mod(NUMANG,nitem))
  endif
  write(*,'(a)') '%FLAG ANGLE_FORCE_CONSTANT'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NUMANG/nitem)-1
    write(*,fmt) (TEQ(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NUMANG,nitem)>0)then
    write(*,fmt) (TEQ(nitem*i+j),j=1,mod(NUMANG,nitem))
  endif
  write(*,'(a)') '%FLAG DIHEDRAL_FORCE_CONSTANT'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPTRA/nitem)-1
    write(*,fmt) (PK(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPTRA,nitem)>0)then
    write(*,fmt) (PK(nitem*i+j),j=1,mod(NPTRA,nitem))
  endif
  write(*,'(a)') '%FLAG DIHEDRAL_PERIODICITY'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPTRA/nitem)-1
    write(*,fmt) (PN(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPTRA,nitem)>0)then
    write(*,fmt) (PN(nitem*i+j),j=1,mod(NPTRA,nitem))
  endif
  write(*,'(a)') '%FLAG DIHEDRAL_PHASE'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPTRA/nitem)-1
    write(*,fmt) (PHASE(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPTRA,nitem)>0)then
    write(*,fmt) (PHASE(nitem*i+j),j=1,mod(NPTRA,nitem))
  endif
  write(*,'(a)') '%FLAG DIHEDRAL_SCALE_FACTOR'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPTRA/nitem)-1
    write(*,fmt) (ONE_SCEE(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPTRA,nitem)>0)then
    write(*,fmt) (ONE_SCEE(nitem*i+j),j=1,mod(NPTRA,nitem))
  endif
  write(*,'(a)') '%FLAG DIHEDRAL_SCALE_FACTOR'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPTRA/nitem)-1
    write(*,fmt) (ONE_SCNB(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPTRA,nitem)>0)then
    write(*,fmt) (ONE_SCNB(nitem*i+j),j=1,mod(NPTRA,nitem))
  endif
  write(*,'(a)') '%FLAG SOLTY'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATYP/nitem)-1
    write(*,fmt) (SOLTY(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NATYP,nitem)>0)then
    write(*,fmt) (SOLTY(nitem*i+j),j=1,mod(NATYP,nitem))
  endif
  write(*,'(a)') '%FLAG LENNARD_JONES_ACOEF'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NTYPES*(NTYPES+1)/2/nitem)-1
    write(*,fmt) (CN1(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NTYPES*(NTYPES+1)/2,nitem)>0)then
    write(*,fmt) (CN1(nitem*i+j),j=1,mod(NTYPES*(NTYPES+1)/2,nitem))
  endif
  write(*,'(a)') '%FLAG LENNARD_JONES_BCOEF'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NTYPES*(NTYPES+1)/2/nitem)-1
    write(*,fmt) (CN2(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NTYPES*(NTYPES+1)/2,nitem)>0)then
    write(*,fmt) (CN2(nitem*i+j),j=1,mod(NTYPES*(NTYPES+1)/2,nitem))
  endif
  fmt='(10i8)'
  nitem=10
  write(*,'(a)') '%FLAG BONDS_INC_HYDROGEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(3*NBONH/nitem)-1
    write(*,fmt) (IJICBH(nitem*i+j),j=1,nitem)
  enddo
  if(mod(3*NBONH,nitem)>0)then
    write(*,fmt) (IJICBH(nitem*i+j),j=1,mod(3*NBONH,nitem))
  endif
  write(*,'(a)') '%FLAG BONDS_WITHOUT_HYDROGEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(3*NBONA/nitem)-1
    write(*,fmt) (IJICB(nitem*i+j),j=1,nitem)
  enddo
  if(mod(3*NBONA,nitem)>0)then
    write(*,fmt) (IJICB(nitem*i+j),j=1,mod(3*NBONA,nitem))
  endif
  write(*,'(a)') '%FLAG ANGLES_INC_HYDROGEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(4*NTHETH/nitem)-1
    write(*,fmt) (IJKICTH(nitem*i+j),j=1,nitem)
  enddo
  if(mod(4*NTHETH,nitem)>0)then
    write(*,fmt) (IJKICTH(nitem*i+j),j=1,mod(4*NTHETH,nitem))
  endif
  write(*,'(a)') '%FLAG ANGLES_WITHOUT_HYDROGEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(4*NTHETA/nitem)-1
    write(*,fmt) (IJKICT(nitem*i+j),j=1,nitem)
  enddo
  if(mod(4*NTHETA,nitem)>0)then
    write(*,fmt) (IJKICT(nitem*i+j),j=1,mod(4*NTHETA,nitem))
  endif
  write(*,'(a)') '%FLAG DIHEDRALS_INC_HYDROGEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(5*NPHIH/nitem)-1
    write(*,fmt) (IJKLICPH(nitem*i+j),j=1,nitem)
  enddo
  if(mod(5*NPHIH,nitem)>0)then
    write(*,fmt) (IJKLICPH(nitem*i+j),j=1,mod(5*NPHIH,nitem))
  endif
  write(*,'(a)') '%FLAG DIHEDRALS_WITHOUT_HYDROGEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(5*NPHIA/nitem)-1
    write(*,fmt) (IJKLICP(nitem*i+j),j=1,nitem)
  enddo
  if(mod(5*NPHIA,nitem)>0)then
    write(*,fmt) (IJKLICP(nitem*i+j),j=1,mod(5*NPHIA,nitem))
  endif
  write(*,'(a)') '%FLAG EXCLUDED_ATOMS_LIST'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NNB/nitem)-1
    write(*,fmt) (INB(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NNB,nitem)>0)then
    write(*,fmt) (INB(nitem*i+j),j=1,mod(NNB,nitem))
  endif
  fmt='(5e16.8)'
  nitem=5
  write(*,'(a)') '%FLAG HBOND_ACOEF'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPHB/nitem)-1
    write(*,fmt) (ASOL(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPHB,nitem)>0)then
    write(*,fmt) (ASOL(nitem*i+j),j=1,mod(NPHB,nitem))
  endif
  write(*,'(a)') '%FLAG HBOND_BCOEF'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPHB/nitem)-1
    write(*,fmt) (BSOL(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPHB,nitem)>0)then
    write(*,fmt) (BSOL(nitem*i+j),j=1,mod(NPHB,nitem))
  endif
  fmt='(20a4)'
  nitem=20
  write(*,'(a)') '%FLAG AMBER_ATOM_TYPE'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (ISYMBL(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NATOM,nitem)>0)then
    write(*,fmt) (ISYMBL(nitem*i+j),j=1,mod(NATOM,nitem))
  endif
  write(*,'(a)') '%FLAG CHAIN_TREE_CLASSIFICATION'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (ITREE(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NATOM,nitem)>0)then
    write(*,fmt) (ITREE(nitem*i+j),j=1,mod(NATOM,nitem))
  endif
  fmt='(5e16.8)'
  nitem=5
  write(*,'(a)') '%FLAG RADII'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (RBORN(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NATOM,nitem)>0)then
    write(*,fmt) (RBORN(nitem*i+j),j=1,mod(NATOM,nitem))
  endif
  write(*,'(a)') '%FLAG SCREEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (FS(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NATOM,nitem)>0)then
    write(*,fmt) (FS(nitem*i+j),j=1,mod(NATOM,nitem))
  endif
endif

endsubroutine
