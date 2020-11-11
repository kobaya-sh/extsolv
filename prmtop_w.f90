subroutine prmtop_w(prmfile_name)
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
!    2. Debug Mode
!
!idbg = .false.
idbg = .true.

write(*,*) 'TEST'
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
  if(NATOM==0)write(*,*)
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
  if(NATOM==0)write(*,*)
  write(*,'(a)') '%FLAG MASS'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (AMASS(nitem*i+j),j=1,nitem)
  enddo
  if(mod(natom,nitem)>0)then
    write(*,fmt) (AMASS(nitem*i+j),j=1,mod(natom,nitem))
  endif
  if(NATOM==0)write(*,*)
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
  if(NATOM==0)write(*,*)
  write(*,'(a)') '%FLAG NUMBER_EXCLUDED_ATOMS'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (NUMEX(nitem*i+j),j=1,nitem)
  enddo
  if(mod(natom,nitem)>0)then
    write(*,fmt) (NUMEX(nitem*i+j),j=1,mod(natom,nitem))
  endif
  if(NATOM==0)write(*,*)
  write(*,'(a)') '%FLAG NONBONDED_PARM_INDEX'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NTYPES**2/nitem)-1
    write(*,fmt) (ICO(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NTYPES**2,nitem)>0)then
    write(*,fmt) (ICO(nitem*i+j),j=1,mod(NTYPES**2,nitem))
  endif
  if(NTYPES==0)write(*,*)
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
  if(NRES==0)write(*,*)
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
  if(NRES==0)write(*,*)
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
  if(NUMBND==0)write(*,*)
  write(*,'(a)') '%FLAG BOND_FORCE_EQUIL'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NUMBND/nitem)-1
    write(*,fmt) (REQ(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NUMBND,nitem)>0)then
    write(*,fmt) (REQ(nitem*i+j),j=1,mod(NUMBND,nitem))
  endif
  if(NUMBND==0)write(*,*)
  write(*,'(a)') '%FLAG ANGLE_FORCE_CONSTANT'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NUMANG/nitem)-1
    write(*,fmt) (TK(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NUMANG,nitem)>0)then
    write(*,fmt) (TK(nitem*i+j),j=1,mod(NUMANG,nitem))
  endif
  if(NUMANG==0)write(*,*)
  write(*,'(a)') '%FLAG ANGLE_FORCE_CONSTANT'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NUMANG/nitem)-1
    write(*,fmt) (TEQ(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NUMANG,nitem)>0)then
    write(*,fmt) (TEQ(nitem*i+j),j=1,mod(NUMANG,nitem))
  endif
  if(NUMANG==0)write(*,*)
  write(*,'(a)') '%FLAG DIHEDRAL_FORCE_CONSTANT'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPTRA/nitem)-1
    write(*,fmt) (PK(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPTRA,nitem)>0)then
    write(*,fmt) (PK(nitem*i+j),j=1,mod(NPTRA,nitem))
  endif
  if(NPTRA==0)write(*,*)
  write(*,'(a)') '%FLAG DIHEDRAL_PERIODICITY'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPTRA/nitem)-1
    write(*,fmt) (PN(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPTRA,nitem)>0)then
    write(*,fmt) (PN(nitem*i+j),j=1,mod(NPTRA,nitem))
  endif
  if(NPTRA==0)write(*,*)
  write(*,'(a)') '%FLAG DIHEDRAL_PHASE'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPTRA/nitem)-1
    write(*,fmt) (PHASE(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPTRA,nitem)>0)then
    write(*,fmt) (PHASE(nitem*i+j),j=1,mod(NPTRA,nitem))
  endif
  if(NPTRA==0)write(*,*)
  write(*,'(a)') '%FLAG DIHEDRAL_SCALE_FACTOR'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPTRA/nitem)-1
    write(*,fmt) (ONE_SCEE(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPTRA,nitem)>0)then
    write(*,fmt) (ONE_SCEE(nitem*i+j),j=1,mod(NPTRA,nitem))
  endif
  if(NPTRA==0)write(*,*)
  write(*,'(a)') '%FLAG DIHEDRAL_SCALE_FACTOR'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPTRA/nitem)-1
    write(*,fmt) (ONE_SCNB(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPTRA,nitem)>0)then
    write(*,fmt) (ONE_SCNB(nitem*i+j),j=1,mod(NPTRA,nitem))
  endif
  if(NPTRA==0)write(*,*)
  write(*,'(a)') '%FLAG SOLTY'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATYP/nitem)-1
    write(*,fmt) (SOLTY(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NATYP,nitem)>0)then
    write(*,fmt) (SOLTY(nitem*i+j),j=1,mod(NATYP,nitem))
  endif
  if(NATYP==0)write(*,*)
  write(*,'(a)') '%FLAG LENNARD_JONES_ACOEF'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NTYPES*(NTYPES+1)/2/nitem)-1
    write(*,fmt) (CN1(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NTYPES*(NTYPES+1)/2,nitem)>0)then
    write(*,fmt) (CN1(nitem*i+j),j=1,mod(NTYPES*(NTYPES+1)/2,nitem))
  endif
  if(NTYPES==0)write(*,*)
  write(*,'(a)') '%FLAG LENNARD_JONES_BCOEF'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NTYPES*(NTYPES+1)/2/nitem)-1
    write(*,fmt) (CN2(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NTYPES*(NTYPES+1)/2,nitem)>0)then
    write(*,fmt) (CN2(nitem*i+j),j=1,mod(NTYPES*(NTYPES+1)/2,nitem))
  endif
  if(NTYPES==0)write(*,*)
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
  if(NBONH==0)write(*,*)
  write(*,'(a)') '%FLAG BONDS_WITHOUT_HYDROGEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(3*NBONA/nitem)-1
    write(*,fmt) (IJICB(nitem*i+j),j=1,nitem)
  enddo
  if(mod(3*NBONA,nitem)>0)then
    write(*,fmt) (IJICB(nitem*i+j),j=1,mod(3*NBONA,nitem))
  endif
  if(NBONA==0)write(*,*)
  write(*,'(a)') '%FLAG ANGLES_INC_HYDROGEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(4*NTHETH/nitem)-1
    write(*,fmt) (IJKICTH(nitem*i+j),j=1,nitem)
  enddo
  if(mod(4*NTHETH,nitem)>0)then
    write(*,fmt) (IJKICTH(nitem*i+j),j=1,mod(4*NTHETH,nitem))
  endif
  if(NTHETH==0)write(*,*)
  write(*,'(a)') '%FLAG ANGLES_INC_HYDROGEN'
  write(*,'(a)') '%FLAG ANGLES_WITHOUT_HYDROGEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(4*NTHETA/nitem)-1
    write(*,fmt) (IJKICT(nitem*i+j),j=1,nitem)
  enddo
  if(mod(4*NTHETA,nitem)>0)then
    write(*,fmt) (IJKICT(nitem*i+j),j=1,mod(4*NTHETA,nitem))
  endif
  if(NTHETA==0)write(*,*)
  write(*,'(a)') '%FLAG DIHEDRALS_INC_HYDROGEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(5*NPHIH/nitem)-1
    write(*,fmt) (IJKLICPH(nitem*i+j),j=1,nitem)
  enddo
  if(mod(5*NPHIH,nitem)>0)then
    write(*,fmt) (IJKLICPH(nitem*i+j),j=1,mod(5*NPHIH,nitem))
  endif
  if(NPHIH==0)write(*,*)
  write(*,'(a)') '%FLAG DIHEDRALS_WITHOUT_HYDROGEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(5*NPHIA/nitem)-1
    write(*,fmt) (IJKLICP(nitem*i+j),j=1,nitem)
  enddo
  if(mod(5*NPHIA,nitem)>0)then
    write(*,fmt) (IJKLICP(nitem*i+j),j=1,mod(5*NPHIA,nitem))
  endif
  if(NPHIA==0)write(*,*)
  write(*,'(a)') '%FLAG EXCLUDED_ATOMS_LIST'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NNB/nitem)-1
    write(*,fmt) (INB(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NNB,nitem)>0)then
    write(*,fmt) (INB(nitem*i+j),j=1,mod(NNB,nitem))
  endif
  if(NNB==0)write(*,*)
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
  if(NPHB==0)write(*,*)
  write(*,'(a)') '%FLAG HBOND_BCOEF'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NPHB/nitem)-1
    write(*,fmt) (BSOL(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NPHB,nitem)>0)then
    write(*,fmt) (BSOL(nitem*i+j),j=1,mod(NPHB,nitem))
  endif
  if(NPHB==0)write(*,*)
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
  if(NATOM==0)write(*,*)
  write(*,'(a)') '%FLAG CHAIN_TREE_CLASSIFICATION'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (ITREE(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NATOM,nitem)>0)then
    write(*,fmt) (ITREE(nitem*i+j),j=1,mod(NATOM,nitem))
  endif
  if(NATOM==0)write(*,*)
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
  if(NATOM==0)write(*,*)
  write(*,'(a)') '%FLAG SCREEN'
  write(*,'(a)') '%FORMAT'//fmt
  do i=0,(NATOM/nitem)-1
    write(*,fmt) (FS(nitem*i+j),j=1,nitem)
  enddo
  if(mod(NATOM,nitem)>0)then
    write(*,fmt) (FS(nitem*i+j),j=1,mod(NATOM,nitem))
  endif
  if(NATOM==0)write(*,*)
endif

return
endsubroutine
