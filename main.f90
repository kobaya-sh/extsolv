program main
implicit none
include 'const.inc'
include 'size.inc'
include 'gau.inc'
include 'prmtop.inc'
include 'mmpara.inc'

! Dipole
real*8          :: tot_charge
real*8          :: center(3)   ,dipole(3)
! Energies
real*8          :: e_bonda     ,e_bondh
real*8          :: e_benda     ,e_bendh
real*8          :: e_torsa     ,e_torsh
real*8          :: e_coulomb   ,e_lj
real*8          :: e_coulomb14a,e_lj14a
real*8          :: e_coulomb14h,e_lj14h
real*8          :: e_ewald
real*8          :: e_tot
! Force
real*8          :: tf_tot(3, m_natom)
! filename
character*60    :: optfile_name,infile_name,outfile_name,msgfile_name
character*60    :: logfile_name,prmfile_name
integer         :: log_level
! Set fh_log=0 or 6 to write log in STDERR or STDOUT ( logfile_name is neglected ).
!integer,parameter :: fh_log = 6
integer         :: fh_log

! counters
integer         :: i,j
logical         :: idbg = .false.

! Functions
real*8,external :: length_vec

real*8          :: tcrd(3, m_natom)

integer         :: natom_gau
!----------------------------------------------------------------------
!
!   0. Initialize
!
!call cpu_time(t1)
call clock_set('main')
call getarg(1,optfile_name)
call getarg(2, infile_name)
call getarg(3,outfile_name)
call clock_look('Initialize')
!----------------------------------------------------------------------
!
!    1. Read Files. 
!
log_level = 0
fh_log = 10
call option ( prmfile_name, logfile_name, optfile_name, log_level, fh_log )
if ((log_level >= 1) .and. (fh_log /= 0) .and. (fh_log /= 6)) then
  open(fh_log,file=trim(logfile_name))
endif
if (log_level >= 1) then
  write(fh_log,*) '    infile_name  = ',trim(infile_name)
  write(fh_log,*) '    optfile_name = ',trim(optfile_name)
  write(fh_log,*) '    prmfile_name = ',trim(prmfile_name)
  write(fh_log,*) '    logfile_name = ',trim(logfile_name)
  write(fh_log,*) '    is_pbc       = ',is_pbc
  if(is_pbc)then
    write(fh_log,*) '    is_ewald     = ',is_ewald
    write(fh_log,*) '    cell_x       = ',cell(1)
    write(fh_log,*) '    cell_y       = ',cell(2)
    write(fh_log,*) '    cell_z       = ',cell(3)
    write(fh_log,*) '    basis_r(a)   = ',basis_r(1,1),basis_r(1,2),basis_r(1,3)
    write(fh_log,*) '    basis_r(b)   = ',basis_r(2,1),basis_r(2,2),basis_r(2,3)
    write(fh_log,*) '    basis_r(c)   = ',basis_r(3,1),basis_r(3,2),basis_r(3,3)
    write(fh_log,*) '    basis_k(a*)  = ',basis_k(1,1),basis_k(1,2),basis_k(1,3)
    write(fh_log,*) '    basis_k(b*)  = ',basis_k(2,1),basis_k(2,2),basis_k(2,3)
    write(fh_log,*) '    basis_k(c*)  = ',basis_k(3,1),basis_k(3,2),basis_k(3,3)
  endif
endif
call prmtop_r(prmfile_name,               &
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
if ( log_level >= 1 ) then
  write(fh_log,*) '    natom        = ',natom
endif
call ein(tcrd, natom_gau, infile_name,deriv_req)
if (natom /= natom_gau) then
  write(*,*) 'Natom is inconsistent (natom_prmtop /= natom_gaussian).'
  write(*,*) '    natom        = ',natom
  write(*,*) '    natom_gau    = ',natom_gau
  write(*,*) 'Wrong prmtop perhaps used.'
  stop
endif
!
!   convert bohr -> angstrom ( amber force field is written in angstrom )
!
tcrd = tcrd * bohr2ang
if (log_level >= 2) then
  write(fh_log,'(a)') '    Input Coordinate ( Angstrom )'
  do i=1,natom
    write(fh_log,'(3f20.12)') tcrd(1:3, i)
  enddo
elseif (log_level >=1) then
  write(fh_log,'(a)') '     Info) Coordinate Output is omitted when log_level < 2.'
endif
call clock_look('Read_Files')
!----------------------------------------------------------------------
!
!    2. Calculate Electric Dipole Moment.
!
call moment(tcrd, charge, natom, tot_charge, center, dipole)
call clock_look('moment')
!---------------------------------------------------------------------- !  !    3. Calculate Energies & Forces.
!
tf_tot = 0.0d0
!tf_bonda = 0.0d0
!tf_bondh = 0.0d0
!tf_benda = 0.0d0
!tf_bendh = 0.0d0
!tf_torsa = 0.0d0
!tf_torsh = 0.0d0
call bond ( tcrd, rk, req, e_bonda, tf_tot, nbona,  ijicb  )
call bond ( tcrd, rk, req, e_bondh, tf_tot, nbonh,  ijicbh )
call bend ( tcrd, tk, teq, e_benda, tf_tot, ntheta, ijkict  )
call bend ( tcrd, tk, teq, e_bendh, tf_tot, ntheth, ijkicth )
call tors ( tcrd, pk, pn, phase, e_torsa, tf_tot, nphia,  ijklicp  )
call tors ( tcrd, pk, pn, phase, e_torsh, tf_tot, nphih,  ijklicph )
call nb14 (tcrd, charge, cn1, cn2, asol, bsol, one_scee, one_scnb, ntypes, ico, iac, &
  e_coulomb14a, e_lj14a, nphia, ijklicp, tf_tot)
call nb14 (tcrd, charge, cn1, cn2, asol, bsol, one_scee, one_scnb, ntypes, ico, iac, &
  e_coulomb14h, e_lj14h, nphih, ijklicph, tf_tot)
call nonbon (tcrd, charge, cn1, cn2, asol, bsol, natom, ntypes, numex, ico, iac, inb, &
  isymbl, e_coulomb, e_lj, tf_tot)
call pme (tcrd, charge, natom, e_ewald, numex, inb, tf_tot)
e_tot = e_bonda + e_bondh + e_benda + e_bendh + e_torsa + e_torsh          &
      + e_coulomb + e_lj + e_coulomb14a + e_lj14a + e_coulomb14h + e_lj14h &
      + e_ewald
call clock_look('integration')
!----------------------------------------------------------------------
!
!   4. Write Log File
!
if (log_level >= 1) then
  write(fh_log,'(a,f20.12,a)') '     Total_charge                = ',tot_charge         ,' e'
  write(fh_log,'(a,f20.12,a)') '     Geometrical Gravity Point   = ',length_vec(center) ,' Angstrom'
  write(fh_log,'(a,f20.12,a)') '       x component               = ',center(1)          ,' Angstrom'
  write(fh_log,'(a,f20.12,a)') '       y component               = ',center(2)          ,' Angstrom'
  write(fh_log,'(a,f20.12,a)') '       z component               = ',center(3)          ,' Angstrom'
  write(fh_log,'(a,f20.12,a)') '     Electric Dipole Moment      = ',length_vec(dipole) ,' e*Angstrom'
  write(fh_log,'(a,f20.12,a)') '       x component               = ',dipole(1)          ,' e*Angstrom'
  write(fh_log,'(a,f20.12,a)') '       y component               = ',dipole(2)          ,' e*Angstrom'
  write(fh_log,'(a,f20.12,a)') '       z component               = ',dipole(3)          ,' e*Angstrom'
  write(fh_log,'(a,f20.12,a)') '     E_bond        (     Total ) = ',e_bonda+e_bondh    ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '       E_bond      ( without H ) = ',e_bonda            ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '       E_bond      ( include H ) = ',e_bondh            ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '     E_bend        (     Total ) = ',e_benda+e_bendh    ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '       E_bend      ( without H ) = ',e_benda            ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '       E_bend      ( include H ) = ',e_bendh            ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '     E_tors        (     Total ) = ',e_torsa+e_torsh    ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '       E_tors      ( without H ) = ',e_torsa            ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '       E_tors      ( include H ) = ',e_torsh            ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '     E_coulomb14   (     Total ) = ', &
                                                              e_coulomb14a+e_coulomb14h ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '       E_coulomb14 ( without H ) = ',e_coulomb14a       ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '       E_coulomb14 ( include H ) = ',e_coulomb14h       ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '     E_LJ14        (     Total ) = ',e_lj14a+e_lj14h    ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '       E_LJ14      ( without H ) = ',e_lj14a            ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '       E_LJ14      ( include H ) = ',e_lj14h            ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '     E_coulomb     (   Not 1-4 ) = ',e_coulomb          ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '     E_LJ          (   Not 1-4 ) = ',e_lj               ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '     E_Ewald       (   Not 1-4 ) = ',e_ewald            ,' kcal/mol'
  write(fh_log,'(a)'         ) '------------------------------------------------------------------------'  
  write(fh_log,'(a,f20.12,a)') '     E_total                     = ',e_tot              ,' kcal/mol'
  write(fh_log,'(a,f20.12,a)') '     E_total                     = ',e_tot/eh2kcal_mol  ,' Hartree'
endif
if (log_level >= 2) then
  write(fh_log,'(3a)') '  Total Force (kcal/(mol*angstrom))'
  do i=1,natom
    write(fh_log,'(3f20.12)') (tf_tot(j, i)/eh2kcal_mol/ang2bohr,j=1,3)
  enddo
elseif (log_level >= 1) then
  write(fh_log,'(a)') '     Info) Force Output is omitted when log_level < 2.'
endif
call clock_look('Logging')
!----------------------------------------------------------------------
!
!    5. Write EOu File
!
e_tot = e_tot / eh2kcal_mol
tf_tot = tf_tot / eh2kcal_mol / ang2bohr
call eou ( natom, outfile_name, deriv_req, e_tot,dipole, tf_tot )

if ((log_level >= 1) .and. (fh_log /= 0) .and. (fh_log /= 6)) then
  close(fh_log)
endif
call clock_look('EOu-writing')
call clock_clear

stop
endprogram
